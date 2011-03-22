package tau.tim.robuttons.jme;

import com.jme3.asset.AssetManager;
import com.jme3.bounding.BoundingBox;
import com.jme3.input.InputManager;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.light.DirectionalLight;
import com.jme3.light.PointLight;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.ShadowMode;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Quad;
import com.jme3.shadow.PssmShadowRenderer;
import com.jme3.texture.Texture;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicReference;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;
import tau.tim.robuttons.core.Coin;
import tau.tim.robuttons.core.Robutton;
import tau.tim.robuttons.math.Vec2;

/**
 * "Klebecode" zwischen der jMonkeyEngine
 *  und dem eigentlichen Simulationsframework
 * @author Tim Taubner
 */
public class JMEGlue {
private final JMEApplication app;
private final Vector3f lightDir = Vector3f.UNIT_X.add(Vector3f.UNIT_Z.mult(-2)).add(Vector3f.UNIT_Y).normalize();
private final ConcurrentMap<Robutton, Node> robs
        = new ConcurrentHashMap<Robutton, Node>();
private final ConcurrentMap<Coin, Spatial> coins
        = new ConcurrentHashMap<Coin, Spatial>();

private final AtomicReference<Spatial> geom_r = new AtomicReference<Spatial>();
private final AtomicReference<Spatial> geom_c = new AtomicReference<Spatial>();
private final AtomicReference<BlockingQueue<Material>> mats =
        new AtomicReference<BlockingQueue<Material>>();

public JMEGlue() {
    this.app = new JMEApplication();
}

public void start() {
    app.enqueue(new Callable<Void>() {
    public Void call() throws Exception {
        /*
        SSAOConfig conf = new SSAOConfig();
        //conf.setScale(200f);
        FilterPostProcessor fpp = new FilterPostProcessor(app.getAssetManager());
        fpp.addFilter(new SSAOFilter(app.getViewPort(), conf));
        fpp.initialize(app.getRenderManager(), app.getViewPort());
        System.out.println("fpp: " + fpp.isInitialized());
        app.getViewPort().addProcessor(fpp);*/

        PssmShadowRenderer sr = //new BasicShadowRenderer(app.getAssetManager(), 1024);
                new PssmShadowRenderer(app.getAssetManager(), 512, 8);
        sr.setDirection(lightDir.clone());
        app.getViewPort().addProcessor(sr);

        DirectionalLight light_d = new DirectionalLight();
        light_d.setDirection(lightDir.clone());
        Node rNode = app.getRootNode();
        rNode.addLight(light_d);

        PointLight light_p = new PointLight();
        light_p.setPosition(new Vector3f(0, 0, 40));
        light_p.setColor(new ColorRGBA(1, 1, 0.888f, 1));
        //rNode.addLight(light_p);
        return null;
    }
    });
    app.start();
}

public void createGameStarter(final ActionListener acl) {
    app.enqueue(new Callable<Void>() {
    public Void call() throws Exception {
        InputManager im = app.getInputManager();
        im.addMapping("startGame", new KeyTrigger(KeyInput.KEY_SPACE));
        im.addListener(acl, "startGame");
        return null;
    }
    });
}

public void createBounds(final Vec2 start, final Vec2 end, final float thick, final double angle) {
    app.enqueue(new Callable<Void>() {
    public Void call() throws Exception {
        Quad q = new Quad((float) (end.x()-start.x()), (float) (end.y()-start.y()));
        Geometry g = new Geometry("Bounds", q);
        g.setLocalTranslation((float) start.x(), (float) start.y(), -0.02f);
        Material mat = app.getAssetManager().loadMaterial("tau/tim/robuttons/data/mat/Bounds.j3m");
        g.setMaterial(mat);
        g.setModelBound(new BoundingBox());
        g.updateModelBound();
        g.setShadowMode(ShadowMode.Receive);
        app.attachChild(g);
        return null;
    }
    });
}

public Material takeOfferCoinMat() {
    try {
        BlockingQueue<Material> q = mats.get();
        if(q == null) {// init:
            q = new LinkedBlockingQueue<Material>(loadCoinMats(app.getAssetManager()));
            System.out.println("q: " + q);
            mats.compareAndSet(null, q);
        }
        Material mat = q.take();
        q.offer(mat);
        return mat;
    } catch (InterruptedException ex) {
        Logger.getLogger(JMEGlue.class.getName()).log(Level.SEVERE, null, ex);
    }
    return null;
}

public void dropCoin(final Coin c) {
    app.enqueue(new Callable<Void>() {
    public Void call() {
        Spatial sp = coins.get(c);
        float r = (float) c.getRadius();
        Vec2 loc = c.getLoc();
        if(sp == null) {
            Spatial g = geom_c.get();
            if(g == null) {
                g = app.getAssetManager().loadModel("tau/tim/robuttons/data/obj/Coin.mesh.xml");
                g.setModelBound(new BoundingBox());
                g.updateModelBound();
                g.setShadowMode(ShadowMode.Receive);
                geom_c.compareAndSet(null, g);
            }
            sp = g.clone();
            sp.setMaterial(takeOfferCoinMat());
        }
        sp.setLocalTranslation((float) loc.x(), (float) loc.y(), 0);
        sp.setLocalScale(r*2);
        app.attachChild(sp);
        coins.put(c, sp);
        return null;
    }
    });
}

public void takeCoin(final Coin c, final Robutton r) {
    app.enqueue(new Callable<Void>() {
    public Void call() {
        Spatial sp = coins.get(c);
        Node n = robs.get(r);
        if (n != null) {
            sp.setLocalTranslation(0, 0, 0);
            n.attachChild(sp);
        } else {
            Node parent = sp.getParent();
            if(parent != null) parent.detachChild(sp);
        }
        return null;
    }
    });
}

public void createRobutton(final Robutton rob) {
    app.enqueue(new Callable<Void>() {
    public Void call() {
        Node rNode = new Node("Rob_Node");
        robs.put(rob, rNode);
        app.attachChild(rNode);
        float r = (float) rob.getRadius();
        if (geom_r.get() == null) {
            Spatial g = app.getAssetManager().loadModel("tau/tim/robuttons/data/obj/Robutton.mesh.xml");
            Material mat = app.getAssetManager().loadMaterial("tau/tim/robuttons/data/mat/Robutton.j3m");
            //Material mat = app.getAssetManager().loadMaterial("Common/Materials/RedColor.j3m");
            g.setModelBound(new BoundingBox());
            g.updateModelBound();
            g.setMaterial(mat);
            g.setShadowMode(ShadowMode.Cast);
            geom_r.compareAndSet(null, g);
        }
        Spatial sp = geom_r.get().clone();
        sp.setLocalScale(r*2);
        rNode.attachChild(sp);
        return null;
    }
    });
}

public void updateRobutton(Robutton r) {
    final Vec2 loc = r.getLoc();
    final Vec2 dir = r.getDir();
    final Spatial sp = robs.get(r);
    if(sp != null) {
        app.enqueue(new Callable<Void>() {
        public Void call() throws Exception {
            sp.setLocalTranslation((float) loc.x(), (float) loc.y(), 0);
            Quaternion q = sp.getLocalRotation();
            q.fromAngleAxis((float) dir.angle(), Vector3f.UNIT_Z);
            sp.setLocalRotation(q);
            return null;
        }
        });
    }
}

public Collection<Material> loadCoinMats(AssetManager aManager) {
    try {
        Properties props = new Properties();
        JarFile jar = new JarFile("./3DRobuttons.jar");
        JarEntry entry = jar.getJarEntry("tau/tim/robuttons/data/mat/coins.properties");
        props.load(jar.getInputStream(entry));
        String tex = props.getProperty("texture");
        int lastPoint = tex.lastIndexOf('.');
        String name = tex.substring(0, lastPoint);
        String suffix = tex.substring(lastPoint+1);

        String[] countries = props.getProperty("countries").split(",");

        List<Material> mats = new LinkedList<Material>();
        for (String country : countries) {
            String countryTex = name + "_" + country;
            Material mat = aManager.loadMaterial("tau/tim/robuttons/data/mat/Coin.j3m").clone();
            mat.setTexture("m_DiffuseMap",  aManager.loadTexture(countryTex + '.' + suffix));
            mat.setTexture("m_SpecularMap", aManager.loadTexture(countryTex + "_spec" + '.' + suffix));
            Texture texture = aManager.loadTexture(countryTex + "_nor" + '.' + suffix);
            if(texture != null) mat.setTexture("m_NormalMap",   texture);
            mats.add(mat);
        }
        return mats;
    } catch (IOException ex) {
        Logger.getLogger(JMEGlue.class.getName()).log(Level.SEVERE, null, ex);
    }
    return Collections.EMPTY_LIST;
}
}

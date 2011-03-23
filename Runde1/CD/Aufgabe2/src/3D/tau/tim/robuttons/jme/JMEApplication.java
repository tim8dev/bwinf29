package tau.tim.robuttons.jme;

import com.jme3.app.SimpleApplication;
import com.jme3.renderer.ViewPort;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 *
 * @author tim
 */
public class JMEApplication extends SimpleApplication {
private final Node root = new Node();
@Override public void simpleInitApp() {
    cam.setLocation(cam.getLocation().addLocal(0, 0, 10));
    flyCam.setMoveSpeed(12f);
    rootNode.attachChild(root);
}

public void attachChild(Spatial s) {
    root.attachChild(s);
}

public ViewPort getViewPort() {
    return viewPort;
}
}

package tau.tim.robuttons.core;

import java.util.concurrent.atomic.AtomicReference;
import tau.tim.robuttons.core.tasks.UpdateTask;
import tau.tim.robuttons.core.tasks.Updatable;
import java.text.NumberFormat;
import tau.tim.robuttons.math.Vec2;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;
import java.util.concurrent.Callable;
import net.jcip.annotations.ThreadSafe;

import static tau.tim.robuttons.math.Utils.*;

/**
 * Robutton mit [fester] Geschwindigkeit und [veränderbarer] Bewegungsrichtung
 * @author Tim Taubner
 */
@ThreadSafe
public class Robutton extends Button implements Updatable {
private final double mps; // Geschwindigkeit in m/s
private final AtomicReference<Vec2> dir = new AtomicReference<Vec2>(X);
private final AtomicReference<Coin> c = new AtomicReference<Coin>();

public Robutton(double radius, Vec2 loc, double mps) {
    super(radius, loc);
    this.mps = mps;
}

public Vec2 getDir() { return dir.get(); }

public Robutton setDir(Vec2 dir) {
    this.dir.set(dir);
    return this;
}
private boolean setDir(Vec2 dir_o, Vec2 dir_n) {
    return dir.compareAndSet(dir_o, dir_n);
}

public void onCollision(Button b) {
         if(b instanceof Robutton) onCollision((Robutton) b);
    else if(b instanceof Coin)     onCollision((Coin) b);
}

// ==== Die Kollisionsregeln:
// ===== Mit Tisch:
public void onCollision(Table t, Vec2 normal) {
    if(normal.angleBetween(dir.get().neg()) < HALF_PI)
        reflect(normal);
}

// ===== Mit anderem Robutton:
private void onCollision(Robutton r) {
    rot(RotAct.ROT180);  // um 180 Grad drehen
    rot(RotAct.ROT_RAND);// nochmal um -90..90 Grad drehen
}

// ===== Mit Münze
private void onCollision(Coin c_n) {
    boolean suc = false;
    do {
        Coin c_o = c.get();
        if (c_o == c_n)
            return;
        if (c_o != null) {
            suc = c.compareAndSet(c_o, null);
            if(suc) {
                c_o.drop(this);
                rot(RotAct.ROT180); // um 180 Grad drehen nach Kollision
            }
        } else {
            suc = c.compareAndSet(c_o, c_n);
            if(suc)
                c_n.take(this);
        }
    } while (!suc);
}

private void reflect(Vec2 normal) {
    boolean suc = false;
    do {
        // Relektion durch Winkelrechnung am Einheitskreis:
        Vec2 dir_new = this.dir.get();
        double rad = normal.neg().angle() - dir_new.angle();
        suc = setDir(dir_new, normal.rot(rad));
    } while(!suc);
}
private void rot(RotAct act) {
    boolean suc = false;
    do {
        Vec2 dir_o = getDir();
        Vec2 dir_n = dir_o.rot(act.radians());
        suc = setDir(dir_o, dir_n);
    } while(!suc);
}
private void step(double secs) {
    boolean suc = false;
    do {
        Vec2 loc = getLoc();
        Vec2 loc_n = loc.add(dir.get().mul(mps*secs).cartesian());
        suc = setLoc(loc, loc_n);
    } while(!suc);
}

private static enum RotAct {
    ROT180  { double radians(){ return PI; } },
    ROT_RAND{ double radians(){ return rand.nextDouble()*PI-HALF_PI; } };
    final Random rand = new Random();
    abstract double radians();
}
private static enum CoinAct{ DROP, TAKE };

public Collection<Callable<Boolean>> updateCalls() {
    Collection<Callable<Boolean>> calls = new ArrayList<Callable<Boolean>>(5);
    calls.add(new UpdateTask(240) { // Der Schrittgeber:
    public boolean update(long millis) {
        step(millis / 1000.0);
        return false;
    }
    });
    return calls;
}

@Override
public boolean collidesWith(Button o) {
    //  Prüfen ob \vec{V}=o.loc-this.loc o, etwa gleiche Richtung wie \vec{dir} hat.
    Vec2 diff = o.getLoc().sub(getLoc());
    if(diff.angleBetween(dir.get()) < HALF_PI) // Wenn Richtungsabweichung nicht < 90°
        if(o instanceof Coin)
            return o.collidesWith(this);
        else
            return super.collidesWith(o);
    return false;
}

//[== Methoden ohne Logik ausgelassen ==]
}

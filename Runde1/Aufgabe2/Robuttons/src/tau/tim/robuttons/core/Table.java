package tau.tim.robuttons.core;

import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.logging.Level;
import java.util.logging.Logger;
import tau.tim.robuttons.core.tasks.Updatable;
import tau.tim.robuttons.core.tasks.UpdateTask;
import tau.tim.robuttons.core.tasks.Updater;
import tau.tim.robuttons.math.Vec2;
import tau.tim.robuttons.shape.Shape;

/**
 * Ein abstrakter Tisch mit Robuttons und Münzen(Coins).
 *
 * @author Tim Taubner
 */
public abstract class Table implements Collidable<Button>, Updatable, Runnable {
private volatile boolean started;

private static final Logger log = Logger.getLogger(Table.class.getCanonicalName());
protected final Collection<Robutton> robs = new CopyOnWriteArraySet<Robutton>();
protected final Collection<Coin> coins = new ConcurrentLinkedQueue<Coin>();

private final Shape shape;

public Table(Shape shape) {
    this.shape = shape;
}

public Shape getShape() {
    return shape;
}

public void addCoin(Coin c, Robutton r) {
    if(log.isLoggable(Level.FINE))
        log.log(Level.FINE, "Coin dropped by {0}.", r);
    coins.add(c);
}

public void remCoin(Coin c, Robutton r) {
    if(log.isLoggable(Level.FINE))
        log.log(Level.FINE, "Coin picked up by {0}.", r);
    coins.remove(c);
}

public void addRobutton(Robutton rob) {
    robs.add(rob);
    if(started)
        createRobCollisionUpdates(rob);
}

public boolean collidesWith(Button r) {
    Vec2 loc = r.getLoc();
    double radius = r.getRadius();
    boolean collided = shape.collidesWith(loc, radius);
    if(collided && r instanceof Robutton)
        onCollision(r);
    return collided;
}
public void onCollision(Button r) {
    if(r instanceof Robutton)
        onCollision((Robutton) r, r.getLoc(), r.getRadius());
}
private void onCollision(Robutton r, Vec2 loc, double radius) {
    r.onCollision(this, reflectNormal(loc, radius));
}
private Vec2 reflectNormal(Vec2 loc, double radius) {
    return shape.reflectNormal(loc, radius);
}

// Starte den Tisch:
public void run() {
    started = true;
    Updater.UPDATER.update(this);
    for (Robutton rob : robs) {
        Updater.UPDATER.update(rob);
        createRobCollisionUpdates(rob);
    }
}

private void createRobCollisionUpdates(final Robutton rob) {
    final Table table = this;
    try {
        Updater.UPDATER.execute(new UpdateTask(800) { // DON'T check too often.
        public boolean update() {
            if (table.collidesWith(rob))
                if (log.isLoggable(Level.FINER))
                    log.log(Level.FINER, "{0} kollidierte mit {1} bei {2}", new Object[]{rob, this, rob.getLoc()});
            return false;
        }
        });
        Updater.UPDATER.execute(new UpdateTask(60) { // DON'T check too often.
        public boolean update() {
            for (Robutton r_1 : robs)
                if (rob.collidesWith(r_1)) {
                    rob.onCollision(r_1);
                    if (log.isLoggable(Level.FINER))
                        log.log(Level.FINER, "{0} kollidierte mit {1} bei {2}", new Object[]{rob, r_1, rob.getLoc()});
                }
            return false;
        }
        });
        Updater.UPDATER.execute(new UpdateTask(600) { // DON'T check too often.
        public boolean update() {
            for (Coin coin : coins)
                if (rob.collidesWith(coin)) {
                    rob.onCollision(coin);
                    if (log.isLoggable(Level.FINER))
                        log.log(Level.FINER, "{0} kollidierte mit {1} bei {2}", new Object[]{rob, coin, rob.getLoc()});
                }
            return false;
        }
        });
    } catch (InterruptedException ex) {
        log.log(Level.SEVERE, null, ex);
    }
}
}

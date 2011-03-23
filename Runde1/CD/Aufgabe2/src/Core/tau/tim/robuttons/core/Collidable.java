package tau.tim.robuttons.core;

/**
 * @author Tim Taubner
 */
public interface Collidable<C extends Collidable> {

public boolean collidesWith(C c);

public void onCollision(C c);
}

package tau.tim.robuttons.core;

import tau.tim.robuttons.math.Vec2;
import java.util.concurrent.atomic.AtomicReference;
import net.jcip.annotations.ThreadSafe;
import tau.tim.robuttons.math.Utils;

/**
 * Ein Abstrakter Button mit Position und Radius.
 * Ist kollidierbar mit anderen Buttons.
 * @author Tim Taubner
 */
@ThreadSafe
public abstract class Button implements Collidable<Button> {

private final double radius;
private final AtomicReference<Vec2> loc =
        new AtomicReference<Vec2>();

public Button(double radius, Vec2 loc) {
    this.radius = radius;
    this.loc.set(loc == null ? Utils.ZERO : loc);
}

public Vec2 getLoc() {
    return loc.get();
}

protected final void setLoc(Vec2 loc) {
    this.loc.set(loc);
}

protected final boolean setLoc(Vec2 loc_o, Vec2 loc_n) {
    return loc.compareAndSet(loc_o, loc_n);
}

public double getRadius() {
    return radius;
}

public boolean collidesWith(Button o) {
    if(this == o) return false; // Nicht mit sich selber kollidierbar.
    double distance = loc.get().sub(o.getLoc()).length();
    return distance < radius + o.getRadius();
}
}

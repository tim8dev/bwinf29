package tau.tim.robuttons.core;

import tau.tim.robuttons.math.Vec2;

/**
 *
 * @author Tim Taubner
 */
public class Coin extends Button implements Comparable<Coin> {
private final Table t;
// Teil des Radius, der nicht zur Kollisionsberechnung benutzt wird.
private final double bound;

public Coin(Table t, double radius, Vec2 loc) {
    this(t, radius, loc, 0.1);
}

public Coin(Table t, double radius, Vec2 loc, double bound) {
    super(radius, loc);
    this.t = t;
    this.bound = bound;
}

// Nix zu tun: Wird aufgenommen und weggeworfen von Robuttons.
public void onCollision(Button c) {}

public void drop(Robutton r) {
    setLoc(r.getLoc());
    t.addCoin(this, r);
}
public void take(Robutton r) {
    t.remCoin(this, r);
}

@Override
public boolean collidesWith(Button o) {
    if(this == o) return false; // Nicht mit sich selber kollidierbar.
    double distance = getLoc().sub(o.getLoc()).length();
    return distance < (getRadius()-bound + o.getRadius());
}

//== Methoden ohne Logik:
@Override
public boolean equals(Object obj) {
    if (obj == null)
        return false;
    if (getClass() != obj.getClass())
        return false;
    return super.equals(obj);
}

@Override
public int hashCode() {
    int hash = 7;
    return hash;
}

public int compareTo(Coin o) {
    return getLoc().compareTo(o.getLoc());
}
}

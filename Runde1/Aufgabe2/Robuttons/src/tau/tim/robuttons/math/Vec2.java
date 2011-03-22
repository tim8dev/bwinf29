package tau.tim.robuttons.math;

import net.jcip.annotations.ThreadSafe;
import net.jcip.annotations.Immutable;

import static java.lang.Math.*;

/**
 * Abstrakte Klasse für alle zweidimensoniale Vektoren.
 * @author Tim Taubner
 */
@Immutable // and thus:
@ThreadSafe
public abstract class Vec2 implements Comparable<Vec2> {

public static Cartese2 cartese(double x, double y) {
    return new Cartese2(x,y);
}
public static Vec2 rand(double length) {
    return new Radial2(random()*Utils.TWO_PI, length);
}
public static Radial2 fromAngle(double radians) {
    return fromAngle(radians, 1);
}
public static Radial2 fromAngle(double radians, double length) {
    return new Radial2(radians, length);
}

public double x() { return cartesian().x(); }
public double y() { return cartesian().y(); }

public Vec2 add(Vec2 o) { return add(o.x(), o.y()); }
public Vec2 sub(Vec2 o) { return sub(o.x(), o.y()); }

public Vec2 mul(double s) { return new Radial2(angle(), length()*s); }
public Vec2 div(double s) { return new Radial2(angle(), length()/s); }

public Vec2 add(double x, double y) { return new Cartese2(x()+x, y()+y); }
public Vec2 sub(double x, double y) { return new Cartese2(x()-x, y()-y); }

public double length() { return radial().length(); }

public Vec2 abs() { return new Cartese2(Math.abs(x()), Math.abs(y())); }
public Vec2 neg() { return new Cartese2(-x(), -y()); }

public Vec2 normal()      {
    if(isNormal()) return this;
    else           return div(length());
}
public boolean isNormal() { return equals(length(), 1); }
public double angle() { return radial().angle(); }
public double angleBetween(Vec2 o) {
    return radial().angleBetween(o.radial());
}

public Vec2 rot(double radians) { return new Radial2(angle()+radians, length()); }

// Längenvergleich:
public int compareTo(Vec2 o) { return Double.compare(length(), o.length()); }

public abstract Cartese2 cartesian();
public abstract Radial2 radial();

//===== Hilfsmethoden (zum Einsatz in Hashtabellen, o.ä.)

@Override
public boolean equals(Object obj) {
    if (obj == null)
        return false;
    if (!(obj instanceof Vec2))
        return false;
    final Vec2 other = (Vec2) obj;
    if (!equals(x(), other.x()))
        return false;
    if (!equals(y(), other.y()))
        return false;
    return true;
}

@Override
public int hashCode() {
    int hash = 7;
    hash = 83 * hash + (int) (Double.doubleToLongBits(this.x()) ^ (Double.doubleToLongBits(this.x()) >>> 32));
    hash = 83 * hash + (int) (Double.doubleToLongBits(this.y()) ^ (Double.doubleToLongBits(this.y()) >>> 32));
    return hash;
}

private static boolean equals(double d, double o) {
    return equals(d, o, 0.1);
}
private static boolean equals(double d, double o, double diff) {
    int i = Double.compare(d, o);
    if(i == 0) return true;
    else if(i > 0) return d - o <= diff;
    else           return o - d <= diff;
}
}

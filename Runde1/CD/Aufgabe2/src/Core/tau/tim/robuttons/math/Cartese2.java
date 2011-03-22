package tau.tim.robuttons.math;

import java.text.NumberFormat;

import static java.lang.Math.*;
import static tau.tim.robuttons.math.Utils.*;

/**
 * Ein Vektor im zweidimensionalem, kartesischem Koordinatensytem.
 * @author Tim Taubner
 */
public final class Cartese2 extends Vec2{
private final double x, y;
private final Radial2 radial;

Cartese2(double x, double y) {
    this(x, y, true);
}
Cartese2(double x, double y, boolean radial) {
    this.x = x;
    this.y = y;
    this.radial = radial ? radial() : null;
}

Cartese2(double x, double y, Radial2 radial) {
    this.x = x;
    this.y = y;
    this.radial = radial;
}

public Cartese2 withRadial() {
    if(radial != null)
        return this;
    return new Cartese2(x, y, true);
}

@Override public double x() { return x; }
@Override public double y() { return y; }

@Override public Cartese2 cartesian() { return this; }

@Override
public Radial2 radial() {
    if (radial == null) {
        double length = sqrt(x*x + y*y);
        double angle = acos(x / length);
        if (y < -0.000001)
            angle = TWO_PI - angle;
        return new Radial2(angle, length, this);
    } else
        return radial;
}
@Override
public Vec2 neg() { return new Cartese2(-x, -y, radial != null); }

//=== optimized methods:
@Override
public double length() {
    if(radial == null)
        return sqrt(x*x+y*y);
    else
        return radial.length();
}

@Override
public String toString() {
    NumberFormat format = NumberFormat.getNumberInstance();
    return "Vec2{x=" + format.format(x) + ",y=" + format.format(y) + '}';
}
}

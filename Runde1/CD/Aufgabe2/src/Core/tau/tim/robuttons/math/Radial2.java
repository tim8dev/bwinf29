package tau.tim.robuttons.math;

import java.text.NumberFormat;

import static java.lang.Math.*;
import static tau.tim.robuttons.math.Utils.*;

/**
 * Ein Vektor im zweidimensoinalem, radialem Koordinatensytem.
 * @author tim
 */
public final class Radial2 extends Vec2 {
private final double length, angle;
private final Cartese2 cartese;

Radial2(double angle, double length) {
    this(angle, length, true);
}
Radial2(double angle, double length, boolean cartese) {
    this.angle = trimAngle(angle);
    this.length = length;
    this.cartese = cartese ? cartesian() : null;
}
Radial2(double angle, double length, Cartese2 cartese) {
    this.angle = trimAngle(angle);
    this.length = length;
    this.cartese = cartese;
}

public Radial2 withCartese() {
    if(cartese != null)
        return this;
    return new Radial2(angle, length, true);
}

@Override public Vec2 mul(double s) { return new Radial2(angle, length*s); }
@Override public Vec2 div(double s) { return new Radial2(angle, length/s); }

@Override public double angle()  { return angle; }
@Override public double length() { return length; }

@Override public Radial2 radial() { return this; }
@Override
public Cartese2 cartesian() {
    if (cartese == null) {
        double x = cos(angle)*length;
        double y = sin(angle)*length;
        return new Cartese2(x, y, this);
    } else
        return cartese;
}

public double angleBetween(Radial2 o) {
    double biggerAng = Math.max(angle, o.angle);
    double smallerAng = Math.min(angle, o.angle);
    double ang = Math.min(TWO_PI-biggerAng+smallerAng, biggerAng-smallerAng);
    return trimAngle(ang);
}

//=== optimized methods:
@Override public double x() {
    if(cartese == null)
        return cos(angle)*length;
    else return cartese.x();
}
@Override public double y() {
    if(cartese == null)
        return sin(angle)*length;
    else return cartese.x();
}
@Override
public Vec2 neg() { return new Radial2(Utils.PI+angle, length, cartese != null); }

@Override
public String toString() {
    NumberFormat format = NumberFormat.getNumberInstance();
    return "Radial2{angle=" + format.format(angle) + ",length=" + format.format(length) + '}';
}
}

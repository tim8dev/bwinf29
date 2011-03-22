package tau.tim.robuttons.shape;

import tau.tim.robuttons.math.Radial2;
import tau.tim.robuttons.math.Vec2;

import static java.lang.Math.*;

/**
 * @author Tim Taubner
 */
public class LineShape implements Shape {
private final Radial2 line;
private final boolean inner;
private final Vec2 normal;

public LineShape(Vec2 line) {
    this(line, true);
}

public LineShape(Vec2 line, boolean inner) {
    this.line = line.radial();
    this.inner = inner;
    Vec2 nor = line.normal();
    this.normal = inner ? nor.neg() : nor;
}

public boolean collidesWith(Vec2 loc, double radius) {
    Radial2 ang = loc.radial();
    double alpha = line.angleBetween(ang);
    double a = cos(alpha) * ang.length();
    // Liegt a 'innerhalb' der Gerade?
    if(inner) return a+radius >= line.length();
    else      return a-radius <= line.length();
}

public double collisionDistance(Vec2 loc, double radius) {
    Radial2 ang = loc.radial();
    double alpha = line.angleBetween(ang);
    double a = cos(alpha) * ang.length();
    // Liegt a 'innerhalb' der Gerade?
    if(inner) {
        if(a+radius >= line.length())
            return (a+radius) - line.length();
    } else {
        if(a-radius <= line.length())
            return line.length() - (a-radius);
    }
    return Double.MAX_VALUE;
}

public Vec2 reflectNormal(Vec2 loc, double radius) {
    assert collidesWith(loc, radius) : "Doesn't even collide!";
    return normal;
}

}

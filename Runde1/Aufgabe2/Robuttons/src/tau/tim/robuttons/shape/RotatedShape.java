package tau.tim.robuttons.shape;

import tau.tim.robuttons.math.Vec2;

/**
 *
 * @author tim
 */
public class RotatedShape implements Shape {

private final double angle;
private final Shape shape;

public RotatedShape(Shape shape, double angle) {
    this.angle = angle;
    this.shape = shape;
}

public double getAngle() {
    return angle;
}

public Shape getShape() {
    return shape;
}

public boolean collidesWith(Vec2 loc, double radius) {
    return shape.collidesWith(loc.rot(angle), radius);
}

public double collisionDistance(Vec2 loc, double radius) {
    return shape.collisionDistance(loc.rot(angle), radius);
}

public Vec2 reflectNormal(Vec2 loc, double radius) {
    return shape.reflectNormal(loc.rot(angle), radius).rot(-angle);
}
}

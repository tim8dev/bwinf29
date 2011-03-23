package tau.tim.robuttons.shape;

import tau.tim.robuttons.math.Vec2;

/**
 *
 * @author Tim Taubner
 */
public interface Shape {

boolean collidesWith(Vec2 loc, double radius);
double collisionDistance(Vec2 loc, double radius);
Vec2 reflectNormal(Vec2 loc, double radius);
}

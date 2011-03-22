package tau.tim.robuttons.shape;

import java.util.Collection;
import tau.tim.robuttons.math.Vec2;

/**
 *
 * @author Tim Taubner
 */
public class CompositeShape implements Shape {
private final Shape[] childs;
private final BoolOp op;

static enum BoolOp {
    OR, // Einer muss kollidieren  (Convex)
    AND;// Alle müssen kollidieren (Concave)
}

static Shape create(BoolOp op, Collection<? extends Shape> shapes) {
    Shape[] shapeArr = new Shape[shapes.size()];
    int i = 0;
    for (Shape shape : shapes) {
        shapeArr[i] = shape;
        i++;
    }
    return new CompositeShape(shapeArr, op);
}

static Shape create(BoolOp op, Shape... shapes) {
    if(shapes.length == 1)
        return shapes[0];
    return new CompositeShape(shapes, op);
}

private CompositeShape(Shape[] childs) {
    this(childs, BoolOp.OR);
}

private CompositeShape(Shape[] childs, BoolOp op) {
    if(op == null)
        throw new IllegalArgumentException("Operator must not be null");
    this.childs = childs;
    this.op = op;
}

public boolean collidesWith(Vec2 loc, double radius) {
    for (Shape shape: childs)
        if (shape.collidesWith(loc, radius)) {
            // Mit einem kollidiert:
            if (op == BoolOp.OR)  return true;
        } else {
            // Mit einem NICHT kollidiert
            if (op == BoolOp.AND) return false;
        }
    // Mit keinem Kollidiert
    if(op == BoolOp.OR) return false;
    // Mit allen Kollidiert
    else                return true;
}

public double collisionDistance(Vec2 loc, double radius) {
    double min = Double.MAX_VALUE;
    for (Shape shape : childs) {
        if (shape.collidesWith(loc, radius)) {
            double distance = shape.collisionDistance(loc, radius);
            min = Math.min(min, distance);
        }
    }
    return min;
}

public Vec2 reflectNormal(Vec2 loc, double radius) {
    double min = Double.MAX_VALUE;
    Shape minShape = null;
    for (Shape shape : childs)
        if (shape.collidesWith(loc, radius)) {
            double distance = shape.collisionDistance(loc, radius);
            if(distance < min) {
                min = distance;
                minShape = shape;
            }
        }
    return minShape.reflectNormal(loc, radius);
}
}

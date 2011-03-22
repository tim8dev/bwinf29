package tau.tim.robuttons.shape;

import java.util.List;
import java.util.LinkedList;
import tau.tim.robuttons.math.Vec2;

import static tau.tim.robuttons.math.Vec2.*;
import static tau.tim.robuttons.math.Utils.*;
import static tau.tim.robuttons.shape.CompositeShape.*;

/**
 * Methoden zur Erzeugung von Formen
 * @author Tim Taubner
 */
public abstract class Shapes {
private Shapes() {}

public static Shape quad(double l, double r, double d, double u) {
    return convex(new LineShape(fromAngle(PI, Math.abs(l)), l <= 0 ? true : false),
                  new LineShape(fromAngle(0,  Math.abs(r)), r >= 0 ? true : false),
                  new LineShape(fromAngle(PI+HALF_PI, Math.abs(d)), d <= 0 ? true : false),
                  new LineShape(fromAngle(   HALF_PI, Math.abs(u)), u >= 0 ? true : false));
}

public static Shape quad(Vec2 start, Vec2 end) {
    return quad(start.x(), end.x(), start.y(), end.y());
}

public static Shape polygon(double counts, double radius) {
    double step = TWO_PI/counts;
    List<LineShape> shapes = new LinkedList<LineShape>();
    for(double angle = 0; angle < TWO_PI; angle += step)
        shapes.add(new LineShape(fromAngle(angle, radius)));
    return CompositeShape.create(BoolOp.OR, shapes);
}

public static Shape rotate(Shape shape, double angle) {
    return new RotatedShape(shape, angle);
}

public static Shape convex(Shape... shapes) {
    return CompositeShape.create(BoolOp.OR, shapes);
}

public static Shape concave(Shape... shapes) {
    return CompositeShape.create(BoolOp.AND, shapes);
}
}

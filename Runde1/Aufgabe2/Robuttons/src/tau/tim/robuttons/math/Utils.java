package tau.tim.robuttons.math;

/**
 * Hilfsmethoden und -konstanten, wie z.B: PI, PI/2 oder X Achse und Y Achse.
 * @author Tim Taubner
 */
public abstract class Utils {
// Not to be extended!:
private Utils() {}


// VECTORS:
public static final Vec2 ZERO = new Cartese2(0, 0).withRadial();
public static final Vec2 X = new Cartese2(1, 0).withRadial();
public static final Vec2 Y = new Cartese2(0, 1).withRadial();

// ANGLES:
public static final double PI = Math.PI, // 180°
                       TWO_PI = PI*2, // 360°
                      FOUR_PI = PI*4, // 720°
                      HALF_PI = PI/2, // 90°
                   QUARTER_PI = PI/4; // 45°
private static final double DEG_TO_RAD = PI / 180;
private static final double RAD_TO_DEG = 180 / PI;
public static double deg(double rad) { return rad * RAD_TO_DEG; }
public static double rad(double deg) { return deg * DEG_TO_RAD; }
private static final Vec2
    vec0   = X,
    vec30  = Vec2.fromAngle(rad(30))   .withCartese(),
    vec60  = Vec2.fromAngle(rad(60))   .withCartese(),
    vec120 = Vec2.fromAngle(rad(120))  .withCartese(),
    vec45  = Vec2.fromAngle(QUARTER_PI).withCartese(),
    vec90  = Y,
    vec180 = Vec2.cartese(-1, 0).withRadial(),
    vec360 = vec0;
public static Vec2 vec(int deg) {
    return deg == 30 ? vec30 :
           deg == 45 ? vec45 :
           deg == 60 ? vec60 :
           deg == 90 ? vec90 :
           deg == 120? vec120:
           deg == 180? vec180:
           Vec2.fromAngle(rad(deg));
}
public static double trimAngle(double angle) {
    angle = angle % TWO_PI;
    if(angle < 0) angle = angle + TWO_PI; // 360° - |angle|
    return angle;
}
}

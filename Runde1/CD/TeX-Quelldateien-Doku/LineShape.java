package tau.tim.robuttons.shape;

import tau.tim.robuttons.math.Radial2;
import tau.tim.robuttons.math.Vec2;

import static java.lang.Math.*;

/**
 * Teilweise vereinfachte Klasse, welche eine Linie repräsentiert.
 * Jedes Polygon kann auf eine Menge Linien abgebildet werden.
 *
 * @author Tim Taubner
 */
public class LineShape implements Shape {
// Der Vektor, welche die Gerade beschreibt:
private final Radial2 line;
// Die Normale in Richtung Ursprung
private final Vec2 normal;

public LineShape(Vec2 line) {
    this.line = line.radial();
    // Geradenvektor normalisieren und negieren (Richtung Ursprung!)
    this.normal = line.normal().neg();
}

public boolean collidesWith(Vec2 loc, double radius) {
    Radial2 ang = loc.radial(); // Radialer Vektor (Kreiskoordinate) der Position
    double alpha = line.angleBetween(ang); // Zwischenwinkel
    // Entfernung vom Ursprung
    double l = ang.length() + radius;
    // a = Teil des Vektors auf der senkrechte der Geraden
    // cos(alpha) = a / l => a = cos(alpha) * l
    double a = cos(alpha) * l;
    // Abstand:
    double dist = a - line.length();
    return dist >= 0; // Ist Radius+Abstand > Abstand der Gerade vom Ursprung?
}

/**
* Gibt die Normale zur Reflektion zurück.
*/
public Vec2 reflectNormal(Vec2 loc, double radius) {
    return normal;
}

}

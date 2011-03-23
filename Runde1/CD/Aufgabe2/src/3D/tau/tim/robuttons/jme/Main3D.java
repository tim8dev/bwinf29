package tau.tim.robuttons.jme;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import tau.tim.robuttons.Main;
import tau.tim.robuttons.core.Coin;
import tau.tim.robuttons.core.Robutton;
import tau.tim.robuttons.core.Table;
import tau.tim.robuttons.math.Utils;
import tau.tim.robuttons.math.Vec2;

/**
 *
 * @author tim
 */
public class Main3D {
public static void main(String[] args) {
    Main.configure(Level.INFO);
    //waveTable(20,8,22,10);
    defTable(50,48,80);
}

public static void defTable(double rad, double v, int robs) {
    double l = -rad, d = -rad, r = rad, u = rad;
    JMETable table = new JMETable(Vec2.cartese(l, d), Vec2.cartese(r, u));
    Main.startTable(table, l, d, r, u, v, robs);
}

public static void waveTable(double size, double v, int columns, int rows) {
    final double rad = 0.5f;
    double l = -2*size, d = -size, r = 2*size, u = size;
    Table t = new JMETable(Vec2.cartese(l, d), Vec2.cartese(r, u));
    // Locations generieren:
    for (double i = l*0.98; i < r*0.4; i+=(r-l)/columns)
        for (double j = u*0.8; j > d*0.8; j-=(u-d)/rows)
            t.addRobutton(new Robutton(rad, Vec2.cartese(i, j), v).setDir(Utils.X));

    for (double i = r*0.; i < r*0.98; i+=(r-l)/columns)
        for (double j = u*0.8; j > d*0.8; j-=(u-d)/rows)
            t.addCoin(new Coin(t, rad, Vec2.cartese(i, j)), null);
    t.run();
}
}

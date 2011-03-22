package tau.tim.robuttons;

import java.math.RoundingMode;
import java.text.NumberFormat;
import java.util.*;
import java.util.logging.*;
import tau.tim.robuttons.core.*;
import tau.tim.robuttons.core.tasks.Updater;
import tau.tim.robuttons.math.*;

/**
 * Starter für eine Simulation, diese protokolliert in der Konsole.
 * @author Tim Taubner
 */
public class Main {
public static void configure(Level level) {
    // DemoTable Logger Konfigurieren:
    Logger log = Logger.getLogger(Table.class.getCanonicalName());
    log.setLevel(level);
    Handler logHandle = new ConsoleHandler();
    logHandle.setLevel(log.getLevel());
    log.addHandler(logHandle);

    // Number Format:
    NumberFormat format = NumberFormat.getNumberInstance();
    format.setGroupingUsed(true);
    format.setMaximumFractionDigits(5);
    format.setRoundingMode(RoundingMode.HALF_UP);

    // Neuen Thread für Updater:
    Updater.UPDATER.newThread();
}

public static void startTable(Table t, double l, double d, double r, double u, double v, int robs) {
    // Robuttons Radius; Geschwindigkeit
    final double rad = .5; // d = 2*r = 1.0
    //final double rad_c = rad*.72; // smaller coins
    final double rad_c = rad;
    // Locations generieren:
    List<Vec2> locList = new ArrayList<Vec2>(50);
    for (double i = l*0.8; i < r*0.8; i+=(r-l)/Math.sqrt(robs*4))
        for (double j = u*0.8; j > d*0.8; j-=(u-d)/Math.sqrt(robs*4))
            locList.add(Vec2.cartese(i, j));
    Random rand = new Random();
    final Queue<Vec2> locs = new LinkedList<Vec2>();
    // shuffle locs:
    while(!locList.isEmpty()) {
        Vec2 loc = locList.remove(rand.nextInt(locList.size()));
        if(loc != null) locs.offer(loc);
    }
    final Queue<Vec2> dirs = new LinkedList<Vec2>();
    // Richtungen generieren:
    for (int i = 0; i < robs; i++) {
        Vec2 vec = Vec2.fromAngle(Math.random() * Utils.HALF_PI); // 0..90°
        //System.out.println("Vec(" + i + "):" + vec);
        dirs.offer(vec);
    }
    for(int i = 0; i < robs; i++) {
        // Neuer Robutton mit r = rad, v = v; Einer zufälligeren Richtung.
        Vec2 dir = dirs.poll();
        //System.out.println("Dir(" + i + ")" + dir);
        t.addRobutton(new Robutton(rad, locs.poll(), v).setDir(dir));
        Coin c = new Coin(t, rad_c, locs.poll());
        if(!t.collidesWith(c)) t.addCoin(c, null);
        c = new Coin(t, rad_c, locs.poll());
        if(!t.collidesWith(c)) t.addCoin(c, null);
    }
    t.run();
}

}

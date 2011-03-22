package tau.tim.robuttons.core.tasks;

import java.util.concurrent.Callable;

/**
 * Eine Aufgabe zum Einreichen im Updater.
 *
 * @author tim
 */
public abstract class UpdateTask implements Callable<Boolean> {
private final long minTime;
private volatile long time;

/**
 * Erzeugt eine Aufgabe, die so oft wie möglich, aber nicht öfter als angegeben
 * ausgeführt wird.
 * @param maxFps Maximale Updatezyklen pro Sekunde
 */
public UpdateTask(int maxFps) {
    this.minTime  = 1000 / maxFps;
    this.time = System.currentTimeMillis();
}

public final Boolean call() throws Exception {
    long curTime = System.currentTimeMillis();
    long millis = curTime - time;
    if (millis < minTime) // Noch warten.
        return Boolean.FALSE;
    boolean b1 = update(millis);
    boolean b2 = update();
    time = curTime;
    return b1 && b2;
}

public boolean update() throws Exception {return true;}
public boolean update(long millis) throws Exception {return true;}
}

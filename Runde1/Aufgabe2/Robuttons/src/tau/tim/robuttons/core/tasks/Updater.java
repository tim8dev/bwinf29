package tau.tim.robuttons.core.tasks;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Der Updater, Kern der Multithreadfunktion,
 * versucht so oft wie möglich zu updaten, und möglichst parallel.
 * @author Tim Taubner
 */
public enum Updater {
    UPDATER;
private static final Logger log = Logger.getLogger(Updater.class.getCanonicalName());
private final int tasksPerThread = 100;
private final AtomicInteger tasks = new AtomicInteger(-20);

private final BlockingQueue<Callable<Boolean>> callQueue =
        new LinkedBlockingQueue<Callable<Boolean>>();

private final ExecutorService executor = Executors.newCachedThreadPool(new ThreadFactory() {
public Thread newThread(Runnable r) {
    Thread t = new Thread(r);
    t.setDaemon(true);
    return t;
}
});

public void newThread() {
    executor.submit(new Callable<Void>() {
    public Void call() throws Exception {
        while (true) { // infinite loop
            Callable<Boolean> call = callQueue.take();
            if (call.call()) tasks.decrementAndGet();
            else             callQueue.add(call);
        }
    }
    });
}

public void update(Updatable updatable) {
    try {
        execute(updatable.updateCalls());
    } catch (InterruptedException ex) {
        log.log(Level.SEVERE, null, ex);
    }
}

public void execute(Collection<Callable<Boolean>> calls) throws InterruptedException {
    if(this.tasks.addAndGet(calls.size())>0)
        startNewThread();
    for (Callable<Boolean> task : calls) callQueue.offer(task, 10, TimeUnit.MILLISECONDS);
}

public void execute(Callable<Boolean>... calls) throws InterruptedException {
    if(this.tasks.addAndGet(calls.length)>0)
        startNewThread();
    for (Callable<Boolean> task : calls) callQueue.offer(task, 10, TimeUnit.MILLISECONDS);
}
public void execute(Callable<Boolean> task) throws InterruptedException {
    if(this.tasks.incrementAndGet()>0) startNewThread();
    callQueue.offer(task, 10, TimeUnit.MILLISECONDS);
}

private final Callable<Void> finiteUpdateCall = new Callable() {
public Void call() throws Exception {
    Callable<Boolean> call;
    do {
        call = callQueue.poll();
        if (call.call()) tasks.decrementAndGet();
        else             callQueue.add(call);
    } while (call != null);
    return null;
}
};

private void startNewThread() {
    this.tasks.addAndGet(-tasksPerThread);
    executor.submit(finiteUpdateCall);
}
}

package tau.tim.robuttons.core.tasks;

import java.util.Collection;
import java.util.concurrent.Callable;

/**
 * Interface für alle Taskerzeuger
 * @author Tim Taubner
 */
public interface Updatable {
    Collection<Callable<Boolean>> updateCalls();
}

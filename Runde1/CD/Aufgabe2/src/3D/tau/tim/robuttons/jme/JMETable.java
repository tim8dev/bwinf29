package tau.tim.robuttons.jme;

import com.jme3.input.controls.ActionListener;
import tau.tim.robuttons.core.tasks.UpdateTask;
import java.util.Collection;
import java.util.LinkedList;
import java.util.concurrent.Callable;
import tau.tim.robuttons.core.Coin;
import tau.tim.robuttons.core.Robutton;
import tau.tim.robuttons.core.Table;
import tau.tim.robuttons.math.Vec2;
import static tau.tim.robuttons.shape.Shapes.*;

/**
 *
 * @author tim
 */
public class JMETable extends Table {
private final JMEGlue glue;

public JMETable(Vec2 start, Vec2 end) {
    super(
            quad(start, end)
         );
    glue = new JMEGlue();
    glue.createBounds(start, end, 0f, 0);
}

@Override
public void addRobutton(Robutton rob) {
    super.addRobutton(rob);
    glue.createRobutton(rob);
}

@Override
public void addCoin(Coin c, Robutton r) {
    super.addCoin(c, r);
    glue.dropCoin(c);
}

@Override
public void remCoin(Coin c, Robutton r) {
    super.remCoin(c, r);
    glue.takeCoin(c, r);
}

private void runSimulation() {
    super.run();
}

@Override
public void run() {
    glue.start();
    glue.createGameStarter(new ActionListener() {
    private volatile boolean started = false;
    public void onAction(String string, boolean bln, float f) {
        if (!started && string.equals("startGame")) {
            started = true;
            runSimulation();
        }
    }
    });
}

@Override
public Collection<Callable<Boolean>> updateCalls() {
    Collection<Callable<Boolean>> calls = new LinkedList<Callable<Boolean>>();
    calls.add(new UpdateTask(30) {
    @Override public boolean update() {
        for (Robutton rob : robs) glue.updateRobutton(rob);
        return false;
    }
    });
    return calls;
}

}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package tau.tim.robuttons.math;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import static tau.tim.robuttons.math.Utils.*;

/**
 *
 * @author tim
 */
public class Radial2Test {

public Radial2Test() {
}

@BeforeClass
public static void setUpClass() throws Exception {
}

@AfterClass
public static void tearDownClass() throws Exception {
}

/**
 * Test of angleBetween method, of class Radial2.
 */
@Test
public void testAngleBetween() {
    System.out.println("angleBetween");
    {
        Radial2 o = vec(30).radial();
        Radial2 instance = vec(270).radial();
        double expResult = rad(120);
        double result = instance.angleBetween(o);
        assertEquals(expResult, result, 0.001);
    }{
        Radial2 o = vec(0).radial();
        Radial2 instance = vec(170).radial();
        double expResult = rad(170);
        double result = instance.angleBetween(o);
        assertEquals(expResult, result, 0.001);
    }{
        Radial2 o = vec(190).radial();
        Radial2 instance = vec(90).radial();
        double expResult = rad(100);
        double result = instance.angleBetween(o);
        assertEquals(expResult, result, 0.001);
    }
}

}

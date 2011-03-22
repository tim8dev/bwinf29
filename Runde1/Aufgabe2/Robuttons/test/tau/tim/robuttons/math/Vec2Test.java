package tau.tim.robuttons.math;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static java.lang.Math.*;
import static org.junit.Assert.*;
import static tau.tim.robuttons.math.Utils.*;

/**
 *
 * @author tim
 */
public class Vec2Test {

public Vec2Test() {
}

@BeforeClass
public static void setUpClass() throws Exception {
}

@AfterClass
public static void tearDownClass() throws Exception {
}

@Before
public void setUp() {
}

@After
public void tearDown() {
}

/**
 * Test of x method, of class Vec2.
 */
@Test
public void testX() {
    System.out.println("x");
    Vec2 instance = Vec2.cartese(12, -90);
    double expResult = 12;
    double result = instance.x();
    assertEquals(expResult, result, 0.001);
}

/**
 * Test of y method, of class Vec2.
 */
@Test
public void testY() {
    System.out.println("y");
    Vec2 instance = Vec2.cartese(12, Utils.PI + 2);
    double expResult = Utils.PI + 2;
    double result = instance.y();
    assertEquals(expResult, result, 0.001);
}

/**
 * Test of length method, of class Vec2.
 */
@Test
public void testLength() {
    System.out.println("length");
    Vec2 instance = Vec2.cartese(1, 1);
    double expResult = sqrt(2);
    double result = instance.length();
    assertEquals(expResult, result, 0.001);
}

/**
 * Test of abs method, of class Vec2.
 */
@Test
public void testAbs() {
    System.out.println("abs");
    Vec2 instance  = Vec2.cartese(-100, -Math.PI);
    Vec2 expResult = Vec2.cartese( 100,  Math.PI);
    Vec2 result = instance.abs();
    assertEquals(expResult, result);
}

/**
 * Test of normal method, of class Vec2.
 */
@Test
public void testNormal() {
    System.out.println("normal");
    Vec2 instance = Vec2.cartese(100, 100);
    Vec2 expResult = Vec2.cartese(1/sqrt(2), 1/sqrt(2));
    Vec2 result = instance.normal();
    assertEquals(expResult.x(), result.x(), 0.001);
    assertEquals(expResult.y(), result.y(), 0.001);
}

/**
 * Test of isNormal method, of class Vec2.
 */
@Test
public void testIsNormal() {
    System.out.println("isNormal");
    {
        Vec2 instance = Vec2.cartese(10, 0);
        boolean expResult = false;
        boolean result = instance.isNormal();
        assertEquals(expResult, result);
    }{
        Vec2 instance = Vec2.cartese(1/sqrt(2), 1/sqrt(2));
        boolean expResult = true;
        boolean result = instance.isNormal();
        assertEquals(expResult, result);
    }{
        Vec2 instance = Vec2.rand(10);
        assertEquals(false, instance.isNormal());
        assertEquals(true, instance.normal().isNormal());
    }
}

/**
 * Test of angle method, of class Vec2.
 */
@Test
public void testAngle() {
    System.out.println("angle");
    {
        Vec2 instance = vec(30);
        double expResult = Math.toRadians(30);
        double result = instance.angle();
        assertEquals("Vector is: " + instance, expResult, result, 0.001);
    }{
        Vec2 instance = vec(91);
        double expResult = Math.toRadians(91);
        double result = instance.angle();
        assertEquals("Vector is: " + instance, expResult, result, 0.001);
    }{
        double radians = Utils.PI*9;
        Vec2 instance = Vec2.fromAngle(radians);
        double expResult = radians % Utils.TWO_PI;
        double result = instance.angle();
        assertEquals("Vector is: " + instance, expResult, result, 0.001);
    }
}

/**
 * Test of rot method, of class Vec2.
 */
@Test
public void testRot() {
    System.out.println("rot");
    double radians = QUARTER_PI;
    Vec2 instance = vec(45);
    Vec2 expResult = vec(90);
    Vec2 result = instance.rot(radians);
    assertEquals(expResult, result);
    assertEquals(result.rot(TWO_PI), result);
    assertEquals(result.rot(Math.PI), result.neg());
    radians = rad(480); // => 120°
    // Rotate about an |angle| > 360°
    assertEquals(instance, instance.rot(radians).rot(-radians));
    // First: Rotate about random angle
    radians = Math.random() * TWO_PI;
    System.out.println("Angle: " + deg(radians));
    assertEquals(instance, instance.rot(radians).rot(-radians));
    // Second: Rotate about random angle
    radians = Math.random() * TWO_PI;
    System.out.println("Angle: " + deg(radians));
    assertEquals(instance, instance.rot(radians).rot(-radians));
}

/**
 * Test of compareTo method, of class Vec2.
 */
@Test
public void testCompareTo() {
    System.out.println("compareTo");
    Vec2 o = Vec2.fromAngle(Math.PI, 500);
    Vec2 instance = Vec2.fromAngle(QUARTER_PI, 3);
    int result = instance.compareTo(o);
    assertTrue(result < 0);
}

}

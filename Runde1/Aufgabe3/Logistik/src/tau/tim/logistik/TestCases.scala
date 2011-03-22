package tau.tim.logistik

import org.junit._
import Assert._

/**
 * Hilfsobjekt zur Testausführung.
 */
object TestCases {
  def main(args: Array[String]): Unit = run
  def run = {
    val cases = new TestCases()
    cases.logistisch1
    cases.logistisch2
    cases.logistisch3
    cases.logistisch4
  }
}

/**
 * Die Unit Tests zu den vorgegebenen Mustereingaben.
 */
class TestCases {
  @Before def setUp:    Unit = {}
  @After  def tearDown: Unit = {}

  def test(table: String, expect: (Int, Int, Int)): Unit = {
    println("Tabelle:\n" + table)
    val result = TourenPlan.berechne(TourenPlan.ausString(table), (x:String) => x)
    assertEquals(result,expect)
  }

  @Test
  def logistisch1 = {
    println("test, Mustereingabe logistisch1")
    val table =
"""
1 2 3 4 5 6
2 3 4 5 6 1
3 4 5 6 7 8
1 2 3 4 5 6
1 2 3 4 5 6
4 3 5 6 1 2
"""
    val expected = (0,11,19)
    test(table,expected)
  }

  @Test
  def logistisch2 = {
    println("test, Mustereingabe logistisch2")
    val table =
"""
2 3 2 1 4 2
1 3 5 2 1 3
1 3 1 1 3 1
2 3 3 1 1 1
4 0 2 7 1 4
5 2 4 6 2 2
"""
    val expected = (0,7,2)
    test(table,expected)
  }

  @Test
  def logistisch3 = {
    println("test, Mustereingabe logistisch3")
    val table =
"""
5 1 2 4 6 0
6 2 0 5 5 0
4 1 1 5 6 1
7 0 1 4 6 0
5 2 2 5 4 0
5 1 0 5 6 1
"""
    val result = TourenPlan.berechne(TourenPlan.ausString(table), (x:String) => x)
    val expected = (1,1,2)
    test(table,expected)
  }

  @Test
  def logistisch4 = {
    println("test, Mustereingabe logistisch4")
    val table =
"""
2 1 1 3 3 2
5 1 2 0 2 2
3 15 6 1 3 4
4 6 1 3 2 15
8 6 2 11 1 6
9 14 2 6 7 2
"""
    val result = TourenPlan.berechne(TourenPlan.ausString(table), (x:String) => x)
    val expected = (42,0,4)
    test(table,expected)
  }

}

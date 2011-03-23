package tau.tim.logistik

import org.junit._
import Assert._

class TestCases {

  @Test
  def test0 = {
    println("test0")
    val tabelle =
      """
1 2 3 4 5 6
2 3 4 5 6 1
3 4 5 6 7 8
1 2 3 4 5 6
1 2 3 4 5 6
4 3 5 6 1 2
"""
    val result = TourenPlan.berechne(TourenPlan.ausString(tabelle), (x:String) => x)
    val expected = (0,11,19)
    assertEquals(result,0)
  }

  @Test
  def test1 = {
    println("test1")
    // TODO: Second Test Case
    val tabelle =
      """
1 2 3 4 5 6
2 3 4 5 6 1
3 4 5 6 7 8
1 2 3 4 5 6
1 2 3 4 5 6
4 3 5 6 1 2
"""
    val result = TourenPlan.berechne(TourenPlan.ausString(tabelle), (x:String) => x)
    val expected = (0,11,19)
    assertEquals(result,0)
  }
}

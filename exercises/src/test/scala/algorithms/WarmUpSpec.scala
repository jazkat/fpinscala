package algorithms

import org.scalatest.WordSpec
import WarmUp._

class WarmUpSpec extends WordSpec{
  val m3 = Vector(
    Vector(11,2, 4),
    Vector(4, 5, 6),
    Vector(10,8,-12))
  val m4 = Vector(
    Vector( -1, 1,-7,-8),
    Vector(-10,-8,-5,-2),
    Vector(  0, 9, 7,-1),
    Vector(  4, 4,-2, 1))

  "BigSum" should {
    "return correct sum for list of large values" in {
      val l = List(1000000001, 1000000002, 1000000003, 1000000004, 1000000005)
      assert(bigSum(l) == 5000000015L)
    }
  }

  "DiagSum" should {
    "return high to low diag sum for n=3 matrix" in {
      assert(diagSum(m3,0,0,0) == 4)
    }
    "return low to high diag sum for n=3 matrix" in {
      assert(diagSum(m3,m3.length-1,0,0) == 19)
    }
    "return high to low diag sum for n=4 matrix" in {
      assert(diagSum(m4,0,0,0) == -1)
    }
    "return low to high diag sum for n=4 matrix" in {
      assert(diagSum(m4,m4.length-1,0,0) == 0)
    }
  }

  "DiagSumDifference" should {
    "return correct value for n=3 matrix" in {
      assert(diagSumDifference(m3) == 15)
    }
    "return correct value for n=4 matrix" in {
      assert(diagSumDifference(m4) == 1)
    }
  }
}

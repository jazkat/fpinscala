import org.scalatest.WordSpec
import fpinscala.gettingstarted.MyModule._
import fpinscala.gettingstarted.PolymorphicFunctions._

class GettingStartedSpec extends WordSpec {

  "Fibonacci sequence" should {
    "compute correct answer for base case n=1" in {
      assert(fib(1) == 1)
    }
    "compute correct answer for n=2" in {
      assert(fib(2) == 1)
    }
    "compute correct answer for n=3" in {
      assert(fib(3) == 2)
    }
    "compute correct answer for n=4" in {
      assert(fib(4)==3)
    }
    "compute correct answer for n=5" in {
      assert(fib(5)==5)
    }
    "compute correct answer for n=6" in {
      assert(fib(6)==8)
    }
    "compute correct answer for n=7" in {
      assert(fib(7)==13)
    }
  }

  "Polymorphic sort check on integers using greater than function" should {
    def gt(a: Int, b: Int) = a > b
    "return true for an empty array of ints" in {
      assert(isSorted(Array(), gt))
    }
    "return true for an array of positive ints of length one" in {
      assert(isSorted(Array(1), gt))
    }
    "return true for an array of negative ints of length one" in {
      assert(isSorted(Array(-5), gt))
    }
    "return true for an ascending array of ints" in {
      assert(isSorted(Array(1, 2, 3), gt))
    }
    "return false for a descending array of ints" in {
      assert(!isSorted(Array(3, 2, 1), gt))
    }
    "return false for an array of ints in random order" in {
      assert(!isSorted(Array(1, 3, 2), gt))
    }
    "return true for an ascending array of ints with duplicates" in {
      assert(isSorted(Array(1, 1, 2, 2, 3, 3), gt))
    }
    "return false for a descending array of ints with duplicates" in {
      assert(!isSorted(Array(3, 3, 2, 2, 1, 1), gt))
    }
    "return true for a non-consecutive ascending array of ints" in {
      assert(isSorted(Array(1, 5, 9000), gt))
    }
    "return false for a non-consecutive descending array of ints" in {
      assert(!isSorted(Array(50, 10, 1), gt))
    }
    "return true for an array of negative and positive ascending ints" in {
      assert(isSorted(Array(-3, -2, -1, 0, 1, 2, 3), gt))
    }
    "return false for an array of negative and positive descending ints" in {
      assert(!isSorted(Array(3, 2, 1, 0, -1, -2, -3), gt))
    }
    "return false for an array of negative and positive ints in random order" in {
      assert(!isSorted(Array(3, -1, 0, 4), gt))
    }
    "return true for an array of negative ascending ints" in {
      assert(isSorted(Array(-5, -4), gt))
    }
    "return false for an array of negative descending ints" in {
      assert(!isSorted(Array(-1, -2, -3), gt))
    }
    "return false for an array of negative ints in random order" in {
      assert(!isSorted(Array(-5, -3, -15), gt))
    }
  }

  "Polymorphic sort check on integers using less than function" should {
    def lt(a: Int, b: Int) = a < b
    "return true for an empty array of ints" in {
      assert(isSorted(Array(), lt))
    }
    "return true for an array of positive ints of length one" in {
      assert(isSorted(Array(1), lt))
    }
    "return true for an array of negative ints of length one" in {
      assert(isSorted(Array(-5), lt))
    }
    "return false for an ascending array of ints" in {
      assert(!isSorted(Array(1, 2, 3), lt))
    }
    "return true for a descending array of ints" in {
      assert(isSorted(Array(3, 2, 1), lt))
    }
    "return false for an array of ints in random order" in {
      assert(!isSorted(Array(1, 3, 2), lt))
    }
    "return false for an ascending array of ints with duplicates" in {
      assert(!isSorted(Array(1, 1, 2, 2, 3, 3), lt))
    }
    "return true for a descending array of ints with duplicates" in {
      assert(isSorted(Array(3, 3, 2, 2, 1, 1), lt))
    }
    "return false for a non-consecutive ascending array of ints" in {
      assert(!isSorted(Array(1, 5, 9000), lt))
    }
    "return true for a non-consecutive descending array of ints" in {
      assert(isSorted(Array(50, 10, 1), lt))
    }
    "return false for an array of negative and positive ascending ints" in {
      assert(!isSorted(Array(-3, -2, -1, 0, 1, 2, 3), lt))
    }
    "return true for an array of negative and positive descending ints" in {
      assert(isSorted(Array(3, 2, 1, 0, -1, -2, -3), lt))
    }
    "return false for an array of negative and positive ints in random order" in {
      assert(!isSorted(Array(3, -1, 0, 4), lt))
    }
    "return false for an array of negative ascending ints" in {
      assert(!isSorted(Array(-5, -4), lt))
    }
    "return true for an array of negative descending ints" in {
      assert(isSorted(Array(-1, -2, -3), lt))
    }
    "return false for an array of negative ints in random order" in {
      assert(!isSorted(Array(-5, -3, -15), lt))
    }
  }

  "Polymorphic sort check on chars using greater than function" should {
    def gt(a: Char, b: Char) = a > b
    "return true for an empty array of chars" in {
      assert(isSorted(Array(), gt))
    }
    "return true for an array of chars of length one" in {
      assert(isSorted(Array('a'), gt))
    }
    "return true for an ascending array of chars" in {
      assert(isSorted(Array('a','b','c'), gt))
    }
    "return false for a descending array of chars" in {
      assert(!isSorted(Array('c','b','a'), gt))
    }
    "return false for an array of chars in random order" in {
      assert(!isSorted(Array('a','c','b'), gt))
    }
    "return true for an ascending array of chars with duplicates" in {
      assert(isSorted(Array('a','a','b','b','c','c'), gt))
    }
    "return false for a descending array of chars with duplicates" in {
      assert(!isSorted(Array('c','c','b','b','a','a'), gt))
    }
    "return true for a non-consecutive ascending array of chars" in {
      assert(isSorted(Array('a','d','z'), gt))
    }
    "return false for a non-consecutive descending array of chars" in {
      assert(!isSorted(Array('y','l','b'), gt))
    }
  }

}
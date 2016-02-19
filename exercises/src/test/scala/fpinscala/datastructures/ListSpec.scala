import org.scalatest.WordSpec
import fpinscala.datastructures._
import fpinscala.datastructures.List._

class ListSpec extends WordSpec {
  def alwaysTrue(i: Int): Boolean = true
  def alwaysFalse(i: Int): Boolean = false
  def isNegative(i: Int): Boolean = i < 0
  def add = (x: Int, y: Int) => x + y
  def multiply = (x: Double, y: Double) => x * y
  def subtract = (x: Int, y: Int) => x - y
  def doubleToString = (x: Double) => x.toString
  def plusOne = (x: Int) => x + 1
  def duplicate = (x: Int) => List(x, x)


  "Tail function" should {
    "produce NoSuchElementException when invoked on empty list" in {
      intercept[NoSuchElementException] {
        tail(Nil)
      }
    }
    "return Nil for list of length 1" in {
      assert(tail(List(1)) == Nil)
    }
    "return the correct tail list for list of length > 1" in {
      assert(tail(List(3,2,1)) == List(2,1))
    }
  }

  "SetHead function" should {
    "produce NoSuchElementException when invoked on empty list" in {
      intercept[NoSuchElementException] {
        setHead(Nil, 1)
      }
    }
    "correctly set head element for list of length 1" in {
      assert(setHead(List(1), 2) == List(2))
    }
    "correctly set head element for list of length > 1" in {
      assert(setHead(List(1,2,3), 0) == List(0,2,3))
    }
  }

  "Drop function" should {
    "return empty list when dropping on empty list" in {
      assert(drop(Nil, 5) == Nil)
    }
    "correctly drop only element of list of length 1" in {
      assert(drop(List(1), 1) == Nil)
    }
    "correctly drop 0 elements of a list" in {
      assert(drop(List(1,2,3), 0) == List(1,2,3))
    }
    "correctly drop the first n elements of list of length > n" in {
      assert(drop(List(1,2,3), 2) == List(3))
    }
    "correctly drop all elements of list of length n" in {
      assert(drop(List(1,2,3), 3) == Nil)
    }
    "return empty list when dropping more elements than length of list" in {
      assert(drop(List(1,2,3), 4) == Nil)
    }
    "safely handle dropping negative number of elements by returning original list" in {
      assert(drop(List(1,2), -2) == List(1,2))
    }
  }

  "DropWhile function" should {
    "return empty list when dropping on empty list" in {
      assert(dropWhile(Nil, isNegative) == Nil)
    }
    "return empty list when dropping all on list of length 1" in {
      assert(dropWhile(List(1), alwaysTrue) == Nil)
    }
    "return original list when dropping none on list of length 1" in {
      assert(dropWhile(List(1), alwaysFalse) == List(1))
    }
    "return empty list when dropping all on list of length > 1" in {
      assert(dropWhile(List(1,2,3), alwaysTrue) == Nil)
    }
    "return original list when dropping none on list of length > 1" in {
      assert(dropWhile(List(1,1,1), alwaysFalse) == List(1,1,1))
    }
    "correctly drop first negative elts of sorted mixed integer list" in {
      assert(dropWhile(List(-3,-2,0,4,7), isNegative) == List(0,4,7))
    }
    "correctly drop first negative elts of unsorted mixed integer list" in {
      assert(dropWhile(List(-3,-5,3,-1,4), isNegative) == List(3,-1,4))
    }
  }

  "Init function" should {
    "produce NoSuchElementException when called on empty list" in {
      intercept[NoSuchElementException] {
        init(Nil)
      }
    }
    "return empty list on list of length 1" in {
      assert(init(List(1)) == Nil)
    }
    "remove last element on list of length > 1" in {
      assert(init(List(1,2,3)) == List(1,2))
    }
  }

  "Length function" should {
    "return 0 when called on empty list" in {
      assert(length(Nil) == 0)
    }
    "return 1 when called on list of length 1" in {
      assert(length(List(5)) == 1)
    }
    "return length when called on list of length > 1" in {
      assert(length(List(5,5,5,5)) == 4)
    }
  }

  "Tail recursive foldRight function" should {
    "return base case when called on empty list with addition" in {
      assert(foldRight2(Nil, 0)(add) == 0)
    }
    "return base case when called on empty list with multiplication" in {
      assert(foldRight2(Nil, 1.0)(multiply) == 1.0)
    }
    "return sum when called on list of length 1 with addition" in {
      assert(foldRight2(List(5), 0)(add) == 5)
    }
    "return product when called on list of length 1 with multiplication" in {
      assert(foldRight2(List(5.0), 1.0)(multiply) == 5.0)
    }
    "return sum when called on list of length > 1 with addition" in {
      assert(foldRight2(List(5, -2, 3), 0)(add) == 6)
    }
    "return product when called on list of length > 1 with multiplication" in {
      assert(foldRight2(List(5.0, -2.5, 0.2), 1.0)(multiply) == -2.5)
    }
    "applies function in the right direction" in {
      assert(foldRight2(List(10, 5), 0)(subtract) == 5)
    }
  }

  "Tail recursive foldLeft function" should {
    "return base case when called on empty list with addition" in {
      assert(foldLeft(Nil, 0)(add) == 0)
    }
    "return base case when called on empty list with multiplication" in {
      assert(foldLeft(Nil, 1.0)(multiply) == 1.0)
    }
    "return sum when called on list of length 1 with addition" in {
      assert(foldLeft(List(5), 0)(add) == 5)
    }
    "return product when called on list of length 1 with multiplication" in {
      assert(foldLeft(List(5.0), 1.0)(multiply) == 5.0)
    }
    "return sum when called on list of length > 1 with addition" in {
      assert(foldLeft(List(5, -2, 3), 0)(add) == 6)
    }
    "return product when called on list of length > 1 with multiplication" in {
      assert(foldLeft(List(5.0, -2.5, 0.2), 1.0)(multiply) == -2.5)
    }
    "applies function in the left direction" in {
      assert(foldLeft(List(10, 5), 0)(subtract) == -5)
    }
  }

  "Reverse function" should {
    "return empty list when called on empty list" in {
      assert(reverse(Nil) == Nil)
    }
    "return original list when called on list of length 1" in {
      assert(reverse(List(1)) == List(1))
    }
    "return reversed list when called on list of length > 1" in {
      assert(reverse(List(1,2,3)) == List(3,2,1))
    }
  }

  "Append function" should {
    "return list with single elt when called on empty list" in {
      assert(append(Nil, 1) == List(1))
    }
    "add new element to end of list when called on list of length 1" in {
      assert(append(List(1), 2) == List(1,2))
    }
    "add new element to end of list when called on list of length > 1" in {
      assert(append(List(1,2,3), 1) == List(1,2,3,1))
    }
  }

  "FlattenLists function" should {
    "return empty list when called on empty list" in {
      assert(flattenLists(Nil) == Nil)
    }
    "return the first list when called on list with only one list" in {
      assert(flattenLists(List(List(1,2,3))) == List(1,2,3))
    }
    "combine a list of three lists of length n > 1" in {
      assert(flattenLists(List(List(1,2,3),List(4,5,6),List(7,8,9))) == List(1,2,3,4,5,6,7,8,9))
    }
    "combine a list of lists that includes an empty list" in {
      assert(flattenLists(List(Nil, List(1,2,3))) == List(1,2,3))
    }
  }

  "AddOne function" should {
    "return empty list when called on empty list" in {
      assert(addOne(Nil) == Nil)
    }
    "return list with incremented elements when called on list of length 1" in {
      assert(addOne(List(0)) == List(1))
    }
    "return list with incremented elements when called on list of length > 1" in {
      assert(addOne(List(1,2,3)) == List(2,3,4))
    }
  }

  "DoubleToStrings function" should {
    "return empty list when called on empty list" in {
      assert(doublesToStrings(Nil) == Nil)
    }
    "convert double to string when called on list of length 1" in {
      assert(doublesToStrings(List(1.0)) == List("1.0"))
    }
    "convert doubles to strings when called on list of length > 1" in {
      assert(doublesToStrings(List(2.53, -1.0)) == List("2.53", "-1.0"))
    }
  }

  "Map function" should {
    "return empty list when called on empty list" in {
      assert(map(Nil)(doubleToString) == Nil)
    }
    "convert double to string when called on list of length 1 with doubleToString" in {
      assert(map(List(2.5))(doubleToString) == List("2.5"))
    }
    "convert doubles to strings when called on list of length > 1 with doubleToString" in {
      assert(map(List(1.0, 0.5, -3.0, 0.0))(doubleToString) == List("1.0", "0.5", "-3.0", "0.0"))
    }
    "add one to element when called on list of length 1 with plusOne" in {
      assert(map(List(5))(plusOne) == List(6))
    }
    "add one to every element when called on list of length > 1 with plusOne" in {
      assert(map(List(5, -5, 0))(plusOne) == List(6, -4, 1))
    }
  }

  "Filter function" should {
    "return empty list when called on empty list" in {
      assert(filter(Nil)(isNegative) == Nil)
    }
    "return original list when filtering none on list of length 1" in {
      assert(filter(List(-5))(isNegative) == List(-5))
    }
    "return empty list when filtering all on list of length 1" in {
      assert(filter(List(5))(isNegative) == Nil)
    }
    "return original list when filtering none on list of length > 1" in {
      assert(filter(List(-5,-4,-7))(isNegative) == List(-5,-4,-7))
    }
    "return empty list when filtering all on list of length > 1" in {
      assert(filter(List(0,5,3))(isNegative) == Nil)
    }
    "return filtered list when filtering negative numbers on list of length > 1 of mixed integers" in {
      assert(filter(List(0,-5,5,-4,3))(isNegative) == List(-5,-4))
    }
  }

  "FlatMap function" should {
    "return empty list when called on empty list" in {
      assert(flatMap(List[Int]())(duplicate) == Nil)
    }
    "return mapped list when called on list of length 1" in {
      assert(flatMap(List(1))(duplicate) == List(1,1))
    }
    "return mapped list when called on list of length > 1" in {
      assert(flatMap(List(1,2,3))(duplicate) == List(1,1,2,2,3,3))
    }
  }

  "Filter2 via flatmap function" should {
    "return empty list when called on empty list" in {
      assert(filter2(Nil)(isNegative) == Nil)
    }
    "return original list when filtering none on list of length 1" in {
      assert(filter2(List(-5))(isNegative) == List(-5))
    }
    "return empty list when filtering all on list of length 1" in {
      assert(filter2(List(5))(isNegative) == Nil)
    }
    "return original list when filtering none on list of length > 1" in {
      assert(filter2(List(-5,-4,-7))(isNegative) == List(-5,-4,-7))
    }
    "return empty list when filtering all on list of length > 1" in {
      assert(filter2(List(0,5,3))(isNegative) == Nil)
    }
    "return filtered list when filtering negative numbers on list of length > 1 of mixed integers" in {
      assert(filter2(List(0,-5,5,-4,3))(isNegative) == List(-5,-4))
    }
  }

  "AddLists function" should {
    "return empty list when called on empty lists" in {
      assert(addLists(Nil, Nil) == Nil)
    }
    "return empty list when called on empty list and list of length 1" in {
      assert(addLists(Nil, List(1)) == Nil)
    }
    "return empty list when called on list of length 1 and empty list" in {
      assert(addLists(List(1), Nil) == Nil)
    }
    "return list of sum when called on lists of length 1" in {
      assert(addLists(List(1), List(2)) == List(3))
    }
    "return list of sums when called on lists of length > 1" in {
      assert(addLists(List(1,2,3), List(2,3,4)) == List(3,5,7))
    }
    "return list of sums with correct length when called on a long list and a short list" in {
      assert(addLists(List(1,2,3), List(3)) == List(4))
    }
    "return list of sums with correct length when called on a short list and a long list" in {
      assert(addLists(List(3), List(1,2,3)) == List(4))
    }
  }

  "ZipWith function" should {
    "return empty list when called on empty lists" in {
      assert(zipWith(Nil, Nil)(add) == Nil)
    }
    "return empty list when called on empty list and list of length 1" in {
      assert(zipWith(Nil, List(1))(add) == Nil)
    }
    "return empty list when called on list of length 1 and empty list" in {
      assert(zipWith(List(1), Nil)(add) == Nil)
    }
    "return list of sum when called on lists of length 1 with addition" in {
      assert(zipWith(List(1), List(2))(add) == List(3))
    }
    "return list of sums when called on lists of length > 1 with addition" in {
      assert(zipWith(List(1,2,3), List(2,3,4))(add) == List(3,5,7))
    }
    "return list of sums with correct length when called on a long list and a short list with addition" in {
      assert(zipWith(List(1,2,3), List(3))(add) == List(4))
    }
    "return list of sums with correct length when called on a short list and a long list with addition" in {
      assert(zipWith(List(3), List(1,2,3))(add) == List(4))
    }
  }

  "HasSubsequence" should {
    "return true for empty list and empty subsequence" in {
      assert(hasSubsequence(Nil, Nil))
    }
    "return false for empty list and non-empty subsequence" in {
      assert(!hasSubsequence(Nil, List(1,2)))
    }
    "return true for non-empty list and empty subsequence" in {
      assert(hasSubsequence(List(1,2),Nil))
    }
    "return true for matching list and subsequence of length 1" in {
      assert(hasSubsequence(List(5),List(5)))
    }
    "return false for not matching list and subsequence of length 1" in {
      assert(!hasSubsequence(List(5),List(1)))
    }
    "return true for list length > 1 that contains subsequence of length 1" in {
      assert(hasSubsequence(List(1,2,3),List(3)))
    }
    "return false for list whose length is shorter than subsequence length" in {
      assert(!hasSubsequence(List(1,2),List(1,2,3)))
    }
    "return true for matching list and subsequence of length > 1" in {
      assert(hasSubsequence(List(1,2,3),List(1,2,3)))
    }
    "return true for list length > 1 that contains subsequence of length > 1" in {
      assert(hasSubsequence(List(1,2,3,4),List(2,3)))
    }
    "return true for list length > 1 that contains subsequence of length > 1 and requires backtracking" in {
      assert(hasSubsequence(List(1,2,3,1,2,3,4,5),List(1,2,3,4)))
    }
  }
}

import org.scalatest.WordSpec
import scala.{Option => _, Some => _, Either => _, None => _}
import fpinscala.errorhandling._
import fpinscala.errorhandling.Option._

class OptionSpec extends WordSpec {
  def isPositive(x: Int) = x >= 0
  def add(x: Int, y: Int) = x + y
  def intToString(x: Int): String = x.toString
  def stringToInt(s: String): Option[Int] = Try(s.toInt)

  "Map function" should {
    "return none when called on none" in {
      assert(None.map(intToString) == None)
    }
    "return some of result of func when called on defined" in {
      assert(Some(5).map(intToString) == Some("5"))
    }
  }

  "GetOrElse function" should {
    "return default when called on none" in {
      assert(None.getOrElse(5) == 5)
    }
    "return value when called on defined" in {
      assert(Some("a").getOrElse("b") == "a")
    }
  }

  "FlatMap function" should {
    "return none when called on none" in {
      assert(None.flatMap(stringToInt) == None)
    }
    "return none when mapped func fails" in {
      assert(Some("a").flatMap(stringToInt) == None)
    }
    "return value option when mapped func applied successfully on defined value" in {
      assert(Some("12").flatMap(stringToInt) == Some(12))
    }
  }

  "OrElse function" should {
    "return second option when called on none" in {
      assert(None.orElse(Some(5) map intToString) == Some("5"))
    }
    "return first option when called on defined int" in {
      assert(Some(5).orElse(Some(1) map intToString) == Some(5))
    }
  }

  "Filter function" should {
    "return none when called on none" in {
      assert(None.filter(isPositive) == None)
    }
    "return none when called on defined int that does not pass filter" in {
      assert(Some(-5).filter(isPositive) == None)
    }
    "return original option when called on defined int that does pass filter" in {
      assert(Some(5).filter(isPositive) == Some(5))
    }
  }

  "Map2 function" should {
    "return none when both args are none" in {
      assert(map2(None,None)(add) == None)
    }
    "return none when first arg is none and second is defined" in {
      assert(map2(None,Some(5))(add) == None)
    }
    "return none when first arg is defined and second is none" in {
      assert(map2(Some(5),None)(add) == None)
    }
    "apply func when both args are defined" in {
      assert(map2(Some(5),Some(6))(add) == Some(11))
    }
  }

  "Sequence function" should {
    "return option empty list when list is empty" in {
      assert(sequence(Nil) == Some(Nil))
    }
    "return none when list is length 1 and contains none" in {
      assert(sequence(List(None)) == None)
    }
    "return option list of val when list is length 1 and contains defined val" in {
      assert(sequence(List(Some(1))) == Some(List(1)))
    }
    "return none when list is length > 1 and contains a single none" in {
      assert(sequence(List(Some(1),Some(2),None,Some(3))) == None)
    }
    "return none when list is length > 1 and contains multiple nones" in {
      assert(sequence(List(None,Some(1),None)) == None)
    }
    "return option list of vals when list is length > 1 and contains all defined vals" in {
      assert(sequence(List(Some(1),Some(2),Some(3))) == Some(List(1,2,3)))
    }
  }

  "Traverse function" should {
    "return option empty list when list is empty" in {
      assert(traverse2(Nil)(stringToInt) == Some(Nil))
    }
    "return none when list is length 1 and result is not defined" in {
      assert(traverse2(List("hi"))(stringToInt) == None)
    }
    "return option list of vals when list is length 1 and result is defined" in {
      assert(traverse2(List("1"))(stringToInt) == Some(List(1)))
    }
    "return none when list is length > 1 and result contains a none" in {
      assert(traverse2(List("1","hi","2"))(stringToInt) == None)
    }
    "return option list of vals when list is length > 1 and result is defined" in {
      assert(traverse2(List("1","2","3"))(stringToInt) == Some(List(1,2,3)))
    }
  }

}

import org.scalatest.WordSpec
import scala.{Option => _, Some => _, Either => _, None => _}
import fpinscala.errorhandling._
import fpinscala.errorhandling.Either._

class EitherSpec extends WordSpec {
  def intToString(x: Int): String = x.toString
  def stringToInt(s: String): Either[String,Int] =
    try Right(s.toInt)
    catch { case e: Exception => Left("Problem!")}

  "Map function" should {
    "return original value on left" in {
      assert(Left("Problem!").map(intToString) == Left("Problem!"))
    }
    "apply function on right" in {
      assert(Right(5).map(intToString) == Right("5"))
    }
  }

  "FlatMap function" should {
    "return original value on left" in {
      assert(Left("Problem!").flatMap(stringToInt) == Left("Problem!"))
    }
    "apply function and flatten on right" in {
      assert(Right("5").flatMap(stringToInt) == Right(5))
    }
    "apply function and flatten on right when func has error" in {
      assert(Right("hi").flatMap(stringToInt) == Left("Problem!"))
    }
  }

  "OrElse function" should {
    "return original value on right" in {
      assert(Right(5).orElse(Right(0)) == Right(5))
    }
    "return provided right value on left" in {
      assert(Left("Problem!").orElse(Right(0)) == Right(0))
    }
    "return provided left value on left" in {
      assert(Left("").orElse(Left("Some kind of problem")) == Left("Some kind of problem"))
    }
  }
}

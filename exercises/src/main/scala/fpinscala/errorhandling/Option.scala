package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  def map2WithFlatmapAndMap[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa,bb)))

  def map2WithForComprehension[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  // downside: discards info about the exception
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  def parseInts(l: List[String]): Option[List[Int]] =
    sequence(l map (i => Try(i.toInt)))

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B]): Option[List[B]] =
      as match {
        case Nil => Some(bs)
        case h::t =>
          f(h) match {
            case None => None
            case Some(b) => loop(t, bs:+b)
          }
      }
    loop(a,List())
  }

  def traverse2[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse2(a)(x => x)
  }
}
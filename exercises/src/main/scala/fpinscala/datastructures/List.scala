package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new NoSuchElementException("Tail() called on empty List")
      case Cons(_,t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new NoSuchElementException("SetHead() on empty List")
      case Cons(_,t) => Cons(h,t)
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x,xs) =>
        if (f(x)) dropWhile(xs, f)
        else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new NoSuchElementException("Init() on empty List")
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, acc) => acc + 1)
  }

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    def loop(xs: List[A], acc: B): B =
      xs match {
        case Nil => acc
        case Cons(h, t) => f(h, loop(t, acc))
      }
    loop(l, z)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(h, z))(f)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())(Cons(_,_))

  def append[A](l: List[A], a: A): List[A] =
    foldRight(l, List(a))(Cons(_,_))

  def mergeLists[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, mergeLists(t, a2))
    }

  def flattenLists[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, List[A]())(mergeLists)
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((h, t) => Cons(h + 1, t))
  }

  def doublesToStrings(l: List[Double]): List[String] = {
    foldRight(l, List[String]())((h, t) => Cons(h.toString, t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((h, t) => Cons(f(h), t))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, List[A]())((h, t) =>
      if (f(h)) Cons(h, filter(t)(f))
      else filter(t)(f))
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    flattenLists(map(l)(f))
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(h => if (f(h)) List(h) else Nil)
  }

  def addLists(xs: List[Int], ys: List[Int]): List[Int] = {
    (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, addLists(t1, t2))
    }
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, _) => sub == Nil
      case (_, Nil) => true
      case (Cons(h,t), Cons(subh,subt)) =>
        // this is meant to be backtracking. TODO need lots of tests to see if this works
        if (h == subh && hasSubsequence(t,subt)) true
        else hasSubsequence(t,sub)
    }
  }
}

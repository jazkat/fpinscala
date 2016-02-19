package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maxElt(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l,r) => maxElt(l) max maxElt(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l,r) => (1 + depth(l)) max (1 + depth(r))
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    }
  }

  // Implement such that fold(t)(Leaf(_))(Branch(_,_)) == t
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l,r) => 1 + l + r)
  }

  def maxElt2(t: Tree[Int]): Int = {
    fold(t)(i => i)((l: Int,r:Int) => l max r)
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((l,r) => (1 + l) max (1 + r))
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(i => Leaf(f(i)):Tree[B])(Branch(_,_))
  }
}
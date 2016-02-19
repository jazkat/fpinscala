import org.scalatest.WordSpec
import fpinscala.datastructures._
import fpinscala.datastructures.Tree._

class TreeSpec extends WordSpec {
  val tree = Branch(Branch(Leaf(3), Leaf(-2)), Branch(Leaf(4), Leaf(0)))
  val negTree = Branch(Branch(Leaf(-3), Leaf(-2)), Branch(Leaf(-4), Leaf(-1)))
  val unbalancedTree = Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Branch(Leaf(1), Leaf(1)))
  def intToString = (x: Int) => x.toString


  "Size function" should {
    "return 1 for leaf" in {
      assert(size(Leaf(5)) == 1)
    }
    "return total number of branches and leaves" in {
      assert(size(tree) == 7)
    }
  }

  "MaxElt function" should {
    "return elt for leaf" in {
      assert(maxElt(Leaf(-5)) == -5)
    }
    "return max value for tree of mixed integers" in {
      assert(maxElt(tree) == 4)
    }
    "return max value for tree of all negative ints" in {
      assert(maxElt(negTree) == -1)
    }
  }

  "Depth function" should {
    "return 0 for leaf" in {
      assert(depth(Leaf(1)) == 0)
    }
    "return depth of binary tree" in {
      assert(depth(tree) == 2)
    }
    "return max depth of unbalanced tree" in {
      assert(depth(unbalancedTree) == 3)
    }
  }

  "Map function" should {
    "apply func to leaf" in {
      assert(map(Leaf(5))(intToString) == Leaf("5"))
    }
    "apply func to binary tree" in {
      assert(map(tree)(intToString) == Branch(Branch(Leaf("3"),Leaf("-2")),Branch(Leaf("4"),Leaf("0"))))
    }
    "apply func to unbalanced tree" in {
      assert(map(unbalancedTree)(intToString) == Branch(Branch(Leaf("1"),Branch(Leaf("1"),Leaf("1"))),Branch(Leaf("1"),Leaf("1"))))
    }
  }

  "Fold function" should {
    "return original tree when called on leaf with Leaf,Branch funcs" in {
      assert(fold(Leaf(5))(Leaf(_):Tree[Int])(Branch(_,_)) == Leaf(5))
    }
    "return original tree when called on tree with Leaf,Branch funcs" in {
      assert(fold(tree)(Leaf(_):Tree[Int])(Branch(_,_)) == tree)
    }
  }

  "Size2 function" should {
    "return 1 for leaf" in {
      assert(size2(Leaf(5)) == 1)
    }
    "return total number of branches and leaves" in {
      assert(size2(tree) == 7)
    }
  }

  "MaxElt2 function" should {
    "return elt for leaf" in {
      assert(maxElt2(Leaf(-5)) == -5)
    }
    "return max value for tree of mixed integers" in {
      assert(maxElt2(tree) == 4)
    }
    "return max value for tree of all negative ints" in {
      assert(maxElt2(negTree) == -1)
    }
  }

  "Depth2 function" should {
    "return 0 for leaf" in {
      assert(depth2(Leaf(1)) == 0)
    }
    "return depth of binary tree" in {
      assert(depth2(tree) == 2)
    }
    "return max depth of unbalanced tree" in {
      assert(depth2(unbalancedTree) == 3)
    }
  }

  "Map2 function" should {
    "apply func to leaf" in {
      assert(map2(Leaf(5))(intToString) == Leaf("5"))
    }
    "apply func to binary tree" in {
      assert(map2(tree)(intToString) == Branch(Branch(Leaf("3"),Leaf("-2")),Branch(Leaf("4"),Leaf("0"))))
    }
    "apply func to unbalanced tree" in {
      assert(map2(unbalancedTree)(intToString) == Branch(Branch(Leaf("1"),Branch(Leaf("1"),Leaf("1"))),Branch(Leaf("1"),Leaf("1"))))
    }
  }

}

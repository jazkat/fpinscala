package algorithms

object WarmUp {

  /**
    * Sum the elements of a list. The elements are expected to be very large values.
   */
  def bigSum(xs: List[Int]): Long = {
    xs.foldLeft(0L)(_ + _)
  }

  /*
  Given a square matrix of N x N integers, calculate the absolute
  difference between the sums of its diagonals.

  Ex:
  11 2 4
  4 5 6
  10 8 -12

  Sum across the primary diagonal: 11 + 5 - 12 = 4
  Sum across the secondary diagonal: 4 + 5 + 10 = 19
  Difference: |4 - 19| = 15
  */
  def diagSumDifference(m: Vector[Vector[Int]]): Int = {
    math.abs(diagSum(m,0,0,0) - diagSum(m,m.length-1,0,0))
  }

  def diagSum(m: Vector[Vector[Int]], row: Int, col: Int, acc: Int): Int = {
    def getNext(i: Int) = if (i == 0) 1 else -1
    val rNext = getNext(row)
    val cNext = getNext(col)
    @annotation.tailrec
    def loop(row: Int, col: Int, acc: Int): Int =
      if (row == m.length || row < 0) acc
      else loop(row+rNext,col+cNext,acc+m(row)(col))
    loop(row,col,0)
  }

}

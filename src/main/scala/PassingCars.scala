/*
A non-empty array A consisting of N integers is given. The consecutive elements of array A represent consecutive cars on a road.

Array A contains only 0s and/or 1s:

0 represents a car traveling east,
1 represents a car traveling west.
The goal is to count passing cars. We say that a pair of cars (P, Q), where 0 ≤ P < Q < N, is passing when P is traveling to the east and Q is traveling to the west.

For example, consider array A such that:

  A[0] = 0
  A[1] = 1
  A[2] = 0
  A[3] = 1
  A[4] = 1
We have five pairs of passing cars: (0, 1), (0, 3), (0, 4), (2, 3), (2, 4).

Write a function:

object Solution { def solution(a: Array[Int]): Int }

that, given a non-empty array A of N integers, returns the number of pairs of passing cars.

The function should return −1 if the number of pairs of passing cars exceeds 1,000,000,000.

For example, given:

  A[0] = 0
  A[1] = 1
  A[2] = 0
  A[3] = 1
  A[4] = 1
the function should return 5, as explained above.

Write an efficient algorithm for the following assumptions:

N is an integer within the range [1..100,000];
each element of array A is an integer that can have one of the following values: 0, 1.
*/

class PassingCars {

  /**
    * 50% O(N ** 2) complexity
    */
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val v = a.toVector

    @scala.annotation.tailrec
    def recurse(v: Vector[Int], sum: Int): Int = {
      if (sum > 1000000000) -1
      else v match {
        case _ +: Seq() => sum
        case x +: xs =>
          if (x == 1) recurse(xs, sum)
          else recurse(xs, sum + xs.sum)
      }
    }

    recurse(v, 0)
  }


  /**
    * 100% O(N) complexity
    */
  def solution2(a: Array[Int]): Int = {
    val v = a.toVector
    val sum = v.sum

    @scala.annotation.tailrec
    def sums(v: Vector[Int], s: Vector[Int], sum: Int): Vector[Int] = {
      v match {
        case Seq() => s
        case x +: xs => sums(xs, s :+ (sum + x), sum + x)
      }
    }

    val inverted = sums(v, Vector(), 0).map(v => sum - v)
    val d = (v zip inverted).filter(_._1 == 0).map(_._2).sum
    if (Math.abs(d) > 1000000000) -1 else d
  }

}

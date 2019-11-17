/* Write a function

object Solution { def solution(a: Array[Int]): Int }

that, given an array A consisting of N integers, returns the number of distinct values in array A.

For example, given array A consisting of six elements such that:

 A[0] = 2    A[1] = 1    A[2] = 1
 A[3] = 2    A[4] = 3    A[5] = 1
the function should return 3, because there are 3 distinct values appearing in array A, namely 1, 2 and 3.

Write an efficient algorithm for the following assumptions:

N is an integer within the range [0..100,000];
each element of array A is an integer within the range [−1,000,000..1,000,000].
Copyright 2009–2019 by Codility Limited. All Rights Reserved. Unauthorized copying, publication or disclosure prohibited.
*/

/* 100% O(N*log(N)) or O(N) complexity */
class Distinct {

  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    if (a.isEmpty) -1
    else {
      val m: (Int, Int) = a.toVector.groupBy(v => v).toVector
        .map(v => (v._1, v._2.length))
        .max(Ordering[Int].on[(_, Int)](_._2))
      if (m._2 <= a.length / 2) -1
      else {
        a.indexOf(m._1)
      }
    }
  }

}

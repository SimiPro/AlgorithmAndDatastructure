package max_partial_sum

/**
  * Created by simipro on 26/02/16.
  */
object MaxPartialSum extends App {
  val array = Array(5, 7, 12, -48, 9, 36, -17, 40, 22, 11, 55, -49, 49, -49, 111, -117)
  val array2 = Array(2, 11, -4, 13) // wrong with prefixes check



  println("max divide&conquer: " + withDivideAndConquer(array, 0, array.length))

  println("max prefix: " + withPrefixSums().toString)
  println("max naive: " + naiveApproach().toString)


  /**
    *
    * @return
    */
  def withDivideAndConquer(input: Array[Int], left: Int, size: Int): Int = {
    if (size == 1) {
      return input(left) // recursion basecase
    }
    val middle: Int = size / 2
    val maxSumLeft = withDivideAndConquer(input, left, middle)
    val maxSumRight = withDivideAndConquer(input, left + middle, size - middle)

    var leftSum, rightSum = Int.MinValue
    var sum = 0

    for (i <- middle until size) {
      sum = sum + input(i)
      rightSum = if (sum > rightSum) sum else rightSum
    }
    sum = 0

    for (i <- middle - 1 to 0 by -1) {
      sum = sum + input(i)
      leftSum = if (sum > leftSum) sum else leftSum
    }

    var maxSum = if (maxSumLeft > maxSumRight) maxSumLeft else maxSumRight
    maxSum = if (maxSum < rightSum + leftSum) rightSum + leftSum else maxSum
    maxSum
  }

  /**
    * first we build the prefix sums
    * afterwards we check the difference of the 2 sums to get the value between i and j
    * O(n.pow(2))
    * @return
    */
  def withPrefixSums(): Int = {
    val S = Array.ofDim[Int](array.length)
    var max = 0

    for (i <- array.indices) {
      if (i > 0) {
        S(i) = S(i - 1) + array(i)
      } else {
        S(i) = array(i)
      }
    }

    for (i <- array.indices) {
      for (j <- array.indices) {
        val currentSum = S(i) - S(j)
        if (currentSum > max) {
          max = currentSum
        }
      }
    }
    max
  }


  /**
    * Naive approache takes every element in array and builds from every element the sums
    * O(n.pow(2))
    * @return max partial sum
    */
  def naiveApproach() = {
    var max = -1000000
    for (i <- array.indices) {
      val elem = array(i)
      if (elem > max) {
        max = elem
      }
      var currentSum = 0
      for (j <- i until array.length) {
        currentSum = currentSum + array(j)
        if (currentSum > max) {
          max = currentSum
        }
      }
    }
    max
  }

}

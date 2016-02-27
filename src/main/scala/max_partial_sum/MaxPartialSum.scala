package max_partial_sum

/**
  * Created by simipro on 26/02/16.
  */
object MaxPartialSum extends App {
  val array2 = Array(5, 7, 12, -48, 9, 36, -17, 40, 22, 11, 55, -49, 49, -49, 111, -117)
  val array = Array(2, 11, -4, 13)



  println("max divide&conquer: " + withDivideAndConquer(array, 0, array.length - 1))

  println("max prefix: " + withPrefixSums().toString)
  println("max naive: " + withPrefixSums().toString)


  /**
    * array size must be power of two
    * @param left
    * @param right
    * @return
    */
  def withDivideAndConquer(input: Array[Int], left: Int, right: Int): Int = {

    if (left == right) {
      // so called basecase
      return input(left)
    }

    val center: Int = (left + right) / 2

    val maxSumLeft = withDivideAndConquer(input, center - 1, center)
    val maxSumRight = withDivideAndConquer(input, center + 1, right)

    var leftBorderSum = 0
    var maxLeftBorderSum = 0

    for (i <- center to left by -1) {
      leftBorderSum = leftBorderSum + input(i)
      if (leftBorderSum > maxLeftBorderSum) {
        maxLeftBorderSum = leftBorderSum
      }
    }

    var rightBorderSum = 0
    var maxRightBorderSum = 0


    for (i <- center to right) {
      rightBorderSum = rightBorderSum + input(i)
      if (maxRightBorderSum < rightBorderSum) {
        maxRightBorderSum = rightBorderSum
      }
    }

    val maxMiddleSum = maxRightBorderSum + maxLeftBorderSum

    var maxSum = if (maxSumLeft > maxSumRight) maxSumLeft else maxSumRight
    maxSum = if (maxSum < maxMiddleSum) maxMiddleSum else maxSum
    maxSum
    /*
        val size = input.length
        val middle = size / 2

        size match {
          case 0 => 0
          case 1 => input(0)
          case _ => {
            var split1 = input.slice(1, middle)
            var split2 = input.slice(middle + 1, size)
            //getSolution(split1)
            //getSolution(split2)
            0
          }
        }

        */

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

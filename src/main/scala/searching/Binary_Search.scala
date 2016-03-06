package searching

/**
  * Created by simipro on 06/03/16.
  */
object Binary_Search {
  implicit val o: Ordering[Int] = new Ordering[Int] {
    // a > b = 1
    // a < b = -1
    // a == b = 0
    override def compare(a: Int, b: Int): Int = {
      if (a < b) {
        -1
      } else if (a > b) {
        1
      } else {
        0
      }
    }
  }

  def main(args: Array[String]) {
    val A = Array(1, 21, 32, 77, 1230, 1900,12000)
    print(search(new SlizeableArray(A), 77))
  }

  def search[T](A: SlizeableArray[T], k: T)(implicit o: Ordering[T]): Int = {
    val m: Int = A.length / 2
    val r = o.compare(k, A(m))
    if (r != 0) return -1
    r match {
      case 1 => search(A.sliceLeft(m), k)
      case -1 => search(A.sliceRight(m), k)
      case 0 => m
    }
  }
}

class SlizeableArray[T](val A:Array[T], var left:Int, var right:Int) {

  def this(A:Array[T]) {
    this(A, 0, A.length)
  }

  // reduce array lenght by z
  def sliceRight(z:Int): SlizeableArray[T] = {
    right = right - z
    this
  }

  // remove first i
  def sliceLeft(i: Int) : SlizeableArray[T] = {
    left = left + i
    this
  }

  def length(): Int = {
    right - left
  }

  def apply(i:Int): T = {
    if (left + i > right || left + i < left) {
      throw new ArrayIndexOutOfBoundsException
    } else {
      A(left + i)
    }
  }


}

trait Ordering[T] {
  // returns 1 if a > b
  // returns -1 if a < b
  // returns 0 if a = b
  def compare(a:T, b:T):Int
}

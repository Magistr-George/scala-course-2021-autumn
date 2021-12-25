package karazin.scala.users.group.week1.homework

import javax.naming.spi.DirStateFactory.Result
import scala.annotation.tailrec
import scala.math.Ordering.String

/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean = if (b) false else true

    def and(left: Boolean, right: Boolean): Boolean = if (left) if (right) true else false else false

    def or(left: Boolean, right: Boolean): Boolean = if (left) true else if (right) true else false

  end `Boolean Operators`

  object `Fermat Numbers`:
    @tailrec
    def mult(left: BigInt, right: BigInt, result: BigInt): BigInt = if (left == 0) result else mult(left - 1, right, result + right)

    val multiplication: (BigInt, BigInt) => BigInt = (left, right) => mult(left, right, 0)

    @tailrec
    def pow1(number: BigInt, step: BigInt, result: BigInt): BigInt = if (step == 0) result else pow1(number, step - 1, multiplication(result, number))

    val power: (BigInt, BigInt) => BigInt = (left, right) => pow1(left, right, 1)

    //Fn = 2 ^ (2 ^ n) + 1, n is non-negative

    val fermatNumber: Int => BigInt = number => power(2, power(2, number)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence`:
    val lookAndSaySequenceElement: Int => BigInt = {
      case 1 => 1
      case 2 => 11
      case n =>
        var str = "11"
        for (i <- 3 to n) {
          str += '%'
          val len = str.length
          var cnt = 1
          var tmp = ""
          val arr = str.toCharArray
          for (j <- 1 until len) {
            if (arr(j) != arr(j - 1)) {
              tmp += cnt + 0
              tmp += arr(j - 1)
              cnt = 1
            }
            else cnt += 1
          }
          str = tmp
        }
        BigInt(str)
    }

  end `Look-and-say Sequence`

  /*object `Kolakoski sequence`:

    def kolakoski(number: Int): Int = {
      var x = -1
      var y = -1
      var i = 0
      while (i < number) {
        i += 1
        var f = y &~ (y + 1)
        x ^= f

        y = (y + 1) | (f & (x >> 1))
      }
      return print(2, 1, x & 1)
    }

  end `Kolakoski sequence`*/

end Homework
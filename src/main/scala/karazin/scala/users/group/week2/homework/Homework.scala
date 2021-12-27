package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational ={
      val numerRes = this.numer*that.denom + this.denom*that.numer
      val denomRes = this.denom*that.denom
      //var res: Rational = new Rational(numerRes,denomRes)

      new Rational(numerRes,denomRes)
    }

    @targetName("negation")
    infix def unary_- : Rational = new Rational(-this.numer,this.denom)

    @targetName("substraction")
    infix def -(that: Rational): Rational = this+(-that)

    @targetName("multiplication")
    infix def *(that: Rational): Rational = new Rational(this.numer*that.numer,this.denom*that.denom)

    @targetName("devision")
    infix def /(that: Rational): Rational = {
      require(that.numer != 0, "Numeretion must be non zero")
      
      var numerRes = this.numer*that.denom
      var denomRes = this.denom*that.numer
      
      if (denomRes < 0 ) {
        denomRes = - denomRes
        numerRes = - numerRes
      }
      new Rational(numerRes,denomRes)
    }

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    override def equals(other: Any): Boolean = other match {
      case r: Rational => this.numer*r.denom==this.denom*r.numer
      case _ =>false
    }

    override def hashCode(): Int = numer*31+denom
      
  end Rational

end Homework
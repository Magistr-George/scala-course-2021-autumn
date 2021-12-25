package karazin.scala.users.group.week3

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int): Nat = int match {
      case 0 => Zero
      case x if x > 0 => Succ(fromInt(x - 1))
      case _ => throw new Exception("Number < 0")
    }
  
    override def toString: String = s"Nat($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    override def successor: Nat = Succ(this)

    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat = throw new Exception("dont subtraction by 0 ")
    
    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean = obj match{
      case n: Zero => true
      case _ =>false
    }

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    def successor: Nat = Succ(this)
    
    infix def +(that: Nat): Nat = that match{
      case n: Zero => this
      case succ: Succ => Succ(this) + succ.predecessor
    }
    
    infix def -(that: Nat): Nat = that match{
      case n: Zero => this
      case succ: Succ => this.predecessor - succ.predecessor
    }
    
    // Optional task
    def toInt: Int =  1 + this.predecessor.toInt

    override def equals(obj: Any): Boolean = obj match {
      case o: Zero => false
      case o: Succ => this.predecessor.equals(o.predecessor)
      case _ => false
    }


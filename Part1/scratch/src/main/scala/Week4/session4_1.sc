import java.io.File

object session4_1 {
  //Peano numbers!
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nat =
      throw new IllegalStateException("No natural less than 0")
    def +(that: Nat): Nat = that
    def -(that: Nat): Nat = if (that.isZero) this else
      throw new IllegalArgumentException("Cannot subtract a number larger than first.")

    override def toString: String = "."
  }

  class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor: Nat = n
    def +(that: Nat): Nat = new Succ(n + that)
    def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

    override def toString: String = "1" + n.toString
  }

  println(Zero.successor.successor.successor.successor + Zero.successor.successor)

  val x = 5 :: 6 :: 7 :: 8 :: List()
  val y = 1 :: 2 :: 4 :: List()

  var a = scala.collection.mutable.Map[Char, Int]()
  var b = scala.collection.mutable.Map[Char, Int](('t', 6))
  a('a') = 5
  a('a') += 1
  a.contains('g')
  a ++ b



  val shit = 5
  shit.toBinaryString


}

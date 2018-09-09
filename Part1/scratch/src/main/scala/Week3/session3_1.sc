object session3_1 {

  val s1 = new NonEmpty(3, new Empty, new Empty)
  val s2 = s1 incl 1
  val s3 = s2 incl 4
  val s4 = s3 incl 5

  val t1 = new NonEmpty(7, new Empty, new Empty)
  val t2 = t1 incl 2
  val t3 = t2 incl 8

  val u1 = s4 union t3
  val u2 = t3 union s4

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

  class Empty extends IntSet {
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
    def contains(x: Int): Boolean = false
    def union(other: IntSet): IntSet = other

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def union(other: IntSet): IntSet =  ((this.left union this.right) union other) incl this.elem
    override def toString: String = "{" + left + elem + right + "}"
  }

}

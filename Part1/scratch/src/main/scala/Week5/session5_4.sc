object session5_4{
  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => y*y :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map ((x: Int) => x*x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val spanned = xs.span(y => y == x)
      spanned._1 :: pack(spanned._2)
    }
  }

  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def encode[T](xs: List[T]): List[(T,Int)] = xs match{
    case Nil => Nil
    case y :: ys => {
      val (fst, scd) = xs.span(a => a==y)
      (y, fst.length) :: encode(scd)
    }
  }

  encode(List("a", "a", "a", "b", "c", "c", "a"))

  val a = 1
  val b = 2
  val c = 3

  val l = List()

  ((l.::(a)).::(b)).::(c)

  a :: b :: c :: l

  l :+ a :+ b :+ c
}
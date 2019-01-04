object session5_1 {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  val a = List(1, 2, 3, 4, 5)
  val b = List(6, 7, 8, 9, 10)
  println(init(a))

  val c = a ::: b
  val d = a ++ b

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ::: List(y)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => if (n == 0) ys ::: removeAt(n - 1, List()) else y :: removeAt(n - 1, ys)
  }

  def flatten(xs: List[Any]): List[Any] = xs match{
    case List()  => xs
    case y :: ys => y match {
      case y: List[Any] => flatten(y) ::: flatten(ys)
      case _       => y :: flatten(ys)
    }
  }
  

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Nil, Nil)          => xs
      case (Nil, b :: bs)      => ys
      case (a :: as, Nil)      => xs
      case (a :: as, b ::  bs) => if (a < b) a :: merge(as, ys)
                                        else b :: merge(xs, bs)
    }


  merge(a, b)

}
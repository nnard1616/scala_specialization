object session5_5{

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( f(_) :: _ )

  val a = List(1,2,3,4)

  mapFun(a, (x: Int) => 2*x)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (x,y) => y +1 )

  lengthFun(a)
}
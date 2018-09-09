object session3_3 {

  def select[T](n: Int, xs: List[T]): T =
    if (n == 0) xs.head
    else if (xs.isEmpty || n < 0) throw new IndexOutOfBoundsException
    else select(n - 1, xs.tail)

  val items = List(1, 2, 3, 4, 5, 6)

  select(-4, items)
}

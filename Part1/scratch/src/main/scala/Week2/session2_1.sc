object session2_1 {

  def sum(f: Int => Int, a: Int, b: Int): Int = {

    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }

  sum((x: Int) => 2 * x, 1, 10)

  def fact(n: Int): Int = if (n==0) 1 else n*fact(n-1)

  fact(5)
}

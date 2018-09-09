object session2_2 {

  //curried and tail recursive
  def prod(f: Int => Int)(a: Int, b: Int): Int = {

    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }

    loop(a, 1)
  }

  prod((x: Int) => 2 * x)(1, 3)

  def factorial(n: Int) = prod(x => x)(1, n)
  factorial(5)

  def mapReduce(m: Int => Int)(
                r: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {

    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, r(acc, m(a)))
    }

    loop(a, init)
  }

  mapReduce((x: Int) => 2 * x)( (i: Int, j: Int) => i*j, 1)(1, 3)
}

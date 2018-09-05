package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int =
      if (n == 0) 1 else n * factorial(n - 1)

    factorial(r) / (factorial(r - c) * factorial(c))
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def count(acc: Int, chars: List[Char]): Int =
      if (chars.isEmpty || acc < 0) acc
      else if (chars.head == '(') count(acc + 1, chars.tail)
      else if (chars.head == ')') count(acc - 1, chars.tail)
      else count(acc, chars.tail)

    count(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    val c = new Array[Int](money + 1)

    c(0) = 1

    for (k <- 1 to money) {
      c(k) = 0
    }

    for (k <- coins) {
      for (i <- 0 to money - k) {
        c(i + k) += c(i)
      }
    }

    c(money)
  }
}

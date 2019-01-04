object session1_5 {
  1 + 2
  def abs(x: Double) = if (x < 0) -x else x
  abs(-5)

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) =
    abs((guess * guess / x) -1) < 0.00001

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)
  sqrt(1e-20)
  sqrt(1e20)
  sqrt(1e50)
}

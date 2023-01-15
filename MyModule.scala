object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // 2.1
  /** Write a recursive function to get the nth fibonacci number. Must use a
    * local tail-recursive function.
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prevAcc: Int, acc: Int): Int =
      if (n <= 0) prevAcc
      else go(n - 1, acc, acc + prevAcc)

    go(n, 0, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }

}

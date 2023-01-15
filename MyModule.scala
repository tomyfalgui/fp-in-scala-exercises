object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)

  }

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

  // 2.2
  /** Implement `isSorted` which checks whether an Array[A] is sorted according
    * to a given comparison function
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false

    loop(0)

  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fib", 11, fib))

    assert(isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x > y) == false)
    assert(isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x < y))
    assert(
      isSorted(
        Array("a", "bc", "def"),
        (x: String, y: String) => x.length < y.length
      )
    )

  }

}

object Chap2 {

  // 2.1
  /** Write a recursive function to get the nth
   *  fibonacci number. Must use a local tail-recursive
   *  function.
   *  */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prevAcc: Int, acc: Int): Int =
      if (n <= 0) prevAcc
      else go(n-1, acc, acc + prevAcc)

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    assert(fib(5) == 5)
  }
    
}

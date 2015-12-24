object MyModule {
  def abs(n: Int): Int = 
    if (n < 0) -n
    else n

  private def formatAbs(x: Int):String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int):String = {
    val msg = "The factorial value of %d is %d"
    msg.format(x, factorial(x))
  }

  private def formatFib(x: Int):String = {
    val msg = "The fib value of %d is %d"
    msg.format(x, fib(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def factorial(n:Int): Int = {
    def go(n:Int , acc: Int): Int = 
      if(n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    def go(n:Int): Int =
      if(n == 0) 0
      else if(n <= 2) 1
      else go(n-1) + go(n-2)

    go(n)
  }

  def isSorted_int(as: Array[Int]): Boolean = {
    @annotation.tailrec
    def loop(n:Int): Boolean =
      if (n >= as.length -1) true
      else if (as(n) > as(n+1)) false
      else loop(n + 1)
      
    loop(0)
  }

  def orderedString(x: String, y:String): Boolean = {
    if (x <= y) false
    else true
  }

  def isSorted[A](as: Array[A], ordered:(A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n:Int): Boolean =
      if (n >= as.length -1) true
      else if (ordered(as(n), as(n+1))) false
      else loop(n + 1)
      
    loop(0)
  }

  def curry[A, B, C](f: (A,B) => C): A => (B => C) =
    (a:A) => ( (b:B) => f(a,b) )

  def uncurry[A, B, C]( f:A => B => C ): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {

    println( isSorted(Array("a","c","b"), orderedString) )

    println(formatFib(8))
    println(formatFactorial(7))
    println(formatAbs(-42))

  }
}

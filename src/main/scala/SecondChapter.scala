import scala.annotation.tailrec

object SecondChapter extends App {

  // Exercise 2.1
  def fibonacci(n: Int): BigInt = {
    @tailrec
    def loop(f: BigInt, s: BigInt, n: Int): BigInt =
      if(n <= 0) s
      else loop(s, f + s, n - 1)
    loop(0, 1, n)
  }

  (1.to(20)).foreach(x => println(fibonacci(x)))
  // Exercise 2.2
  def isSorted[A](a: Array[A], f: (A,A) => Boolean) = {
    @tailrec
    def loop(h: A, a: Array[A]): Boolean =
      if(a.length <= 1) true
      else f(h, a.head) && loop(a.head, a.tail)
    loop(a.head, a.tail)
  }

  println(isSorted(Array(3,1,2).sorted, (a:Int, b:Int) => a < b ))

  // Exercise 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}

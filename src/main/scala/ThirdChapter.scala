import scala.annotation.tailrec

object ThirdChapter extends App {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(l: List[Int]): Int = l match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of an empty list")
      case Cons(_, xs) => xs
    }

    // Exercise 3.3
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

    // Exercise 3.4
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else l match {
      case Nil => sys.error("cannot drop element on empty list")
      case Cons(_, t) => drop(t, n - 1)
    }

    // Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if(f(h)) => dropWhile(t, f)
      case _ => l
    }

    def append[A](a: List[A], b: List[A]): List[A] = a match {
      case Nil => b
      case Cons(h, t) => Cons(h, append(t, b))
    }

  }

  // Exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(x)

  val aList = List(1,2,3,4,5)
  val bList = List(1,2,3,4,5,6,7)

  println(List.tail(aList))
  println(List.setHead(aList, 0))
  println(List.drop(aList, 3))
  println(List.dropWhile(bList, (x: Int) => x % 5 != 0))
  println(List.append(aList, bList))
}


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

    def append[A](a: List[A], e: A): List[A] = a match {
      case Nil => Cons(e, Nil)
      case Cons(h, t) => Cons(h, append(t, e))
    }

    // Exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("Nil list have no elements")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h, t) if(f(h)) => dropWhile2(t)(f)
      case _ => l
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // foldRight from Standard Library
    def head[A](l: List[A]): A = l match {
      case Nil => sys.error("head of a Nil list")
      case Cons(h, _) => h
    }

    def isEmpty[A](l: List[A]): Boolean = l match {
      case Nil => true
      case Cons(_,_) => false
    }

    def reverseIt[A](l: List[A]): List[A] = {
      var result: List[A] = Nil
      var these: List[A] = l
      while (!isEmpty(these)) {
        result = Cons(head(these), result)
        these = tail(these)
      }
      result
    }

    def foldRightIt[A,B](as: List[A], z: B)(op: (A, B) => B): B = {
      var acc = z
      var these = reverseIt(as)
      while (!isEmpty(these)) {
        acc = op(head(these), acc)
        these = tail(these)
      }
      acc
    }

    // Exercise 3.7
    // Not possible

    // Exercise 3.8
    // Gets same list

    // Exercise 3.9
    def length[A](l: List[A]): Int = foldRight(l, 0)((_: A, a: Int) => a + 1)

    def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)
    def product2(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)
    def sumIt(l: List[Int]): Int = foldRightIt(l, 0)(_ + _)
    def productIt(l: List[Double]): Double = foldRightIt(l, 1.0)(_ * _)
  }
  import ThirdChapter.List._

  // Exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)
  val aList = List(1,2,3,4,5)
  val bList = List(1,2,3,4,5,6,7)
  val dList: List[Double] = List(1,2,3,4,5)



  println(tail(aList))
  println(setHead(aList, 0))
  println(drop(aList, 3))
  println(dropWhile(bList, (x: Int) => x % 5 != 0))
  println(dropWhile2(bList)(x => x < 4))
  println(append(aList, bList))
  println(init(aList))
  println(sumIt(aList))
  println(productIt(dList))
  println(reverseIt(aList))
  println(foldRight(aList, Nil:List[Int])(Cons(_, _)))
  println(length(bList))

}

List(1,2,3).foldRight(0)(_ + _)
List(1,2,3).drop(3)
List(1,2,3).dropWhile(_ < 2)
List(1,2,3).foldLeft(0)(_ + _)

List(List(1,2,3), List(4,5)).foldRight(Nil:List[Int])(_ appendedAll _)


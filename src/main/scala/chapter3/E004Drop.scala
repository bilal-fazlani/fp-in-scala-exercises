package chapter3

import scala.annotation.tailrec

object E004Drop extends App {

  sealed trait List[+A]

  case class Cons[+A](x: A, xs: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    def apply[A](t: A*): List[A] =
      if (t.isEmpty) Nil
      else Cons(t.head, apply(t.tail: _*))

    def tail[T](list: List[T]) = list match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    def setHead[T](list: List[T], x: T) = list match {
      case Cons(y, ys) => Cons(x, ys)
      case Nil => Nil
    }

    def headOption[T](list: List[T]) = list match {
      case Nil => None
      case Cons(x, _) => Some(x)
    }
    
    def drop[T](list:List[T], n:Int): List[T] = list match {
      case Nil => Nil
      case x if n == 0 => x
      case Cons(x, xs) => drop(xs, n-1)
    }
  }

}

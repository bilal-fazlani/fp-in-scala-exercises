package chapter3

import scala.annotation.tailrec

object E005_1Append extends App {

  sealed trait List[+A] {
    override def toString: String =
      this match {
        case c @ Cons(x, xs) => "List(...)"
        case Nil             => "List()"
      }
  }

  case class Cons[+A](x: A, xs: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    def apply[A](t: A*): List[A] =
      if (t.isEmpty) Nil
      else Cons(t.head, apply(t.tail: _*))

    def tail[T](list: List[T]) =
      list match {
        case Nil         => Nil
        case Cons(x, xs) => xs
      }

    def setHead[T](list: List[T], x: T) =
      list match {
        case Cons(y, ys) => Cons(x, ys)
        case Nil         => Nil
      }

    def headOption[T](list: List[T]) =
      list match {
        case Nil        => None
        case Cons(x, _) => Some(x)
      }

    @tailrec
    def drop[T](list: List[T], n: Int): List[T] =
      list match {
        case Nil         => Nil
        case x if n == 0 => x
        case Cons(x, xs) => drop(xs, n - 1)
      }

    @tailrec
    def dropWhile[T](list: List[T], f: T => Boolean): List[T] =
      list match {
        case Nil                       => Nil
        case l @ Cons(x, xs) if (f(x)) => dropWhile(xs, f)
        case l @ Cons(x, xs)           => l
      }

    def append[A](list1: List[A], list2: List[A]): List[A] =
      list1 match {
        case Nil         => list2
        case Cons(x, xs) => Cons(x, append(xs, list2))
      }

    def append[A](list1: List[A], elem: A): List[A] = append(list1, List(elem))

  }
}

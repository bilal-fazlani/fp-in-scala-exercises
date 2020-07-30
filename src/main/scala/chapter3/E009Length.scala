package chapter3

import scala.annotation.tailrec

object E009Length extends App {

  sealed trait List[+A] {
    override def toString: String = {
      def loop(remaining: List[A], first: Boolean = false): String =
        remaining match {
          case Nil         => ""
          case Cons(x, xs) => (if (first) "" else ", ") + x.toString + loop(xs)
        }
      "List(" + loop(this, first = true) + ")"
    }
  }

  case class Cons[+A](x: A, xs: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    def apply[A](t: A*): List[A] =
      if (t.isEmpty) Nil
      else Cons(t.head, apply(t.tail: _*))

    def unapply[T](arg: List[T]): Option[(T, List[T])] =
      arg match {
        case Cons(x, xs) => Some((x, xs))
        case Nil         => None
      }

    def tail[T](list: List[T]): List[T] =
      list match {
        case Nil         => Nil
        case Cons(x, xs) => xs
      }

    def setHead[T](list: List[T], x: T): List[T] =
      list match {
        case Cons(y, ys) => Cons(x, ys)
        case Nil         => Nil
      }

    def headOption[T](list: List[T]): Option[T] =
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
        case Nil =>
          list2
        case Cons(x, xs) =>
          Cons(x, append(xs, list2))
      }

    def append[A](list1: List[A], elem: A): List[A] = append(list1, List(elem))

    def init[A](list: List[A]): List[A] = {
      @tailrec
      def loop(soFar: List[A], remaining: List[A]): List[A] =
        remaining match {
          case Nil | Cons(_, Nil) =>
            Nil
          case Cons(x, Cons(_, Nil)) =>
            append(soFar, x)
          case Cons(x, xs) =>
            val newSoFar = append(soFar, x)
            loop(newSoFar, xs)
        }
      loop(Nil, list)
    }

    def foldRight[A, B](list: List[A], zero: B)(f: (A, B) => B): B =
      list match {
        case Nil         => zero
        case Cons(x, xs) => f(x, foldRight(xs, zero)(f))
      }

    def length[A](list: List[A]): Int = foldRight(list, 0)((_, x) => x + 1)

    def sum(list: List[Int]): Int     = foldRight(list, 0)(_ + _)
    def product(list: List[Int]): Int = foldRight(list, 1)(_ * _)
  }
}

package chapter3

import scala.annotation.tailrec

object E001SumProduct extends App {
  sealed trait List[+A]

  case class Cons[+A](x: A, xs: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    def sum(ints: List[Int]): Int =
      ints match {
        case Cons(x, xs) => x + sum(xs)
        case Nil         => 0
      }

    def product(ints: List[Int]): Int =
      ints match {
        case Cons(0, _)  => 0
        case Cons(x, xs) => x * product(xs)
        case Nil         => 1
      }
  }
}

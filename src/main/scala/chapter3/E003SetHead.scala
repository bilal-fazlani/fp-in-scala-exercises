package chapter3

object E003SetHead extends App {

  sealed trait List[+A]

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

    def setHead[T](list: List[T], x: T): List[T] =
      list match {
        case Cons(_, ys) => Cons(x, ys)
        case Nil         => Nil
      }

    def headOption[T](list: List[T]) =
      list match {
        case Nil        => None
        case Cons(x, _) => Some(x)
      }
  }

}

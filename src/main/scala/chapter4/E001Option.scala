package chapter4

object E001Option {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case Some(value: A) => Some(f(value))
        case None           => None
      }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this match {
        case Some(value: A) => f(value)
        case None           => None
      }
    }

    def filter(f: A => Boolean): Option[A] =
      this.flatMap(a => if (f(a)) this else None)
  }

  case class Some[+A](value: A) extends Option[A]
  case object None              extends Option[Nothing]
}

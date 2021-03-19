package chapter4

object E001Option {
  trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case Some(value: A) => Some(f(value))
        case None           => None
      }
  }

  case class Some[+A](value: A) extends Option[A]
  case object None              extends Option[Nothing]
}

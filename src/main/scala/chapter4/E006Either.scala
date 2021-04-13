package chapter4

object E006Either {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      if (isRight) Right(f(get)) else this.asInstanceOf[Left[E]]
// --  with pattern matching
//      this match {
//        case Right(value) => Right(f(value))
//        case e @ Left(_)  => e
//      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      if (isRight) f(get) else this.asInstanceOf[Left[E]]
// --  with pattern matching
//      this match {
//        case Right(value) => f(value)
//        case e @ Left(_)  => e
//      }
    }

    def orElse[EE >: E, B >: A](value: => Either[EE, B]): Either[EE, B] =
      if (isRight) this else value

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(a => b.map(b1 => f(a, b1)))

    def isRight: Boolean
    def get: A
  }
  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def isRight: Boolean = false
    override def get: Nothing     = throw new RuntimeException("get called on left")
  }
  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def isRight: Boolean = true
    override def get: A           = value
  }
}

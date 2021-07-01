package chapter4

object E008CollectErrors {
  sealed trait Validated[+E, +A] {
    def map[B](f: A => B): Validated[E, B] = {
      if (isSuccess) Valid(f(get)) else this.asInstanceOf[Invalid[E]]
    }

    def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = {
      if (isSuccess) f(get) else this.asInstanceOf[Invalid[E]]
    }

    def or[EE >: E, B >: A](value: => Validated[EE, B]): Validated[EE, B] = {
      (this, value) match {
        case (v @ Valid(_), _)              => v
        case (_, v @ Valid(_))              => v
        case (Invalid(err1), Invalid(err2)) => Invalid(err1 ++ err2)
      }
    }

    def filter[EE >: E](f: A => Boolean, zero: EE): Validated[EE, A] = {
      this.map(f).flatMap(if (_) this else Invalid(Seq(zero)))
    }

    def orElse[EE >: E, B >: A](value: => Validated[EE, B]): Validated[EE, B] =
      if (isSuccess) this else value

    def map2[EE >: E, B, C](
        b: Validated[EE, B]
    )(f: (A, B) => C): Validated[EE, C] = {
      (this, b) match {
        case (Valid(value1), Valid(value2))       => Valid(f(value1, value2))
        case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
        case (_, invalid @ Invalid(_))            => invalid
        case (invalid @ Invalid(_), _)            => invalid
      }
    }

    def isSuccess: Boolean
    def get: A
    def errors: Seq[E]
  }
  case class Valid[+A](value: A) extends Validated[Nothing, A] {
    lazy val isSuccess: Boolean = true
    lazy val get: A             = value
    def errors: Seq[Nothing]    = Seq.empty
  }
  case class Invalid[+E](override val errors: Seq[E])
      extends Validated[E, Nothing] {
    lazy val isSuccess: Boolean = false
    lazy val get: Nothing       = throw new RuntimeException("get called on failure")
  }
  object Invalid {
    def apply[E](error: E): Invalid[E] = Invalid(Seq(error))
  }
  object Validated {
    def pure[A](value: A): Valid[A] = Valid(value)

    def traverse[E, A, B](seq: Seq[Validated[E, A]])(
        f: A => B
    ): Validated[E, Seq[B]] = {
      seq.foldLeft[Validated[E, Seq[B]]](Valid(Seq.empty)) {
        case (Valid(acc), Valid(c))               => Valid(acc :+ f(c))
        case (Invalid(pErrors), Invalid(cErrors)) => Invalid(pErrors ++ cErrors)
        case (_, invalid @ Invalid(_))            => invalid
        case (invalid @ Invalid(_), _)            => invalid
      }
    }

    def sequence[E, A](seq: Seq[Validated[E, A]]): Validated[E, Seq[A]] =
      traverse(seq)(identity)
  }
}

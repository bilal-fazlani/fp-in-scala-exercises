package chapter4

object E001Option {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      if (isDefined) Some(f(get)) else None

    def flatMap[B](f: A => Option[B]): Option[B] =
      if (isDefined) f(get) else None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      if (isDefined) this else ob

    def getOrElse[B >: A](default: => B): B = if (isDefined) get else default

    def filter(f: A => Boolean): Option[A] =
      this.flatMap(a => if (f(a)) this else None)

    val isDefined: Boolean
    def get: A
  }

  case class Some[+A](value: A) extends Option[A] {
    override val isDefined: Boolean = true
    override lazy val get: A        = value
  }
  case object None extends Option[Nothing] {
    override val isDefined: Boolean = false
    override lazy val get: Nothing  = throw new RuntimeException("no value")
  }
}

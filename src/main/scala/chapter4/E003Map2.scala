package chapter4

object E003Map2 {
  def map2[A, B, C](
      a: Option[A],
      b: Option[B]
  )(f: (A, B) => C): Option[C] =
    for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)
}

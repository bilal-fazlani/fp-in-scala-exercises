package chapter4

import chapter4.E006Either._

object E007EitherSequenceTraverse {
  def traverse[A, B, E](seq: Seq[A])(
      f: A => Either[E, B]
  ): Either[E, Seq[B]] =
    seq.foldLeft[Either[E, Seq[B]]](Right(Seq.empty)) {
      case (acc, cur) => acc.flatMap(ac => f(cur).map(ac.appended))
    }

  def sequence[A, E](seq: Seq[Either[E, A]]): Either[E, Seq[A]] =
    traverse(seq)(identity)
}

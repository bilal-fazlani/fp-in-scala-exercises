package chapter4

object E005OptionTraverse {
  def traverse[A, B](seq: Seq[A])(f: A => Option[B]): Option[Seq[B]] =
    seq.foldLeft[Option[Seq[B]]](Some(Seq.empty[B])) {
      case (acc, a) => acc.flatMap(ac => f(a).map(ac.appended))
    }
}

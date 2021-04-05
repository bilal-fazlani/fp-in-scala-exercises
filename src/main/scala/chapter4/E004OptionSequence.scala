package chapter4

object E004OptionSequence {
  def sequence[A](seq: Seq[Option[A]]): Option[Seq[A]] =
    seq.foldLeft[Option[Seq[A]]](Some(Seq.empty[A])) {
      case (acc, cur) =>
        for {
          ac <- acc
          cc <- cur
        } yield ac.appended(cc)
    }
}

package chapter4

import munit.FunSuite

import chapter4.E006Either._

class E007EitherSequenceTraverseTest extends FunSuite {
  case class TraverseTestCase(
      input: Seq[String],
      output: Either[String, Seq[Int]]
  ) {
    override def toString: String = s"Traverse: $input => $output"
  }

  val parseInt: String => Either[String, Int] = str =>
    str.toIntOption
      .fold[Either[String, Int]](Left("decoding error"))(Right.apply)

  val traverseCases: Seq[TraverseTestCase] = Seq(
    TraverseTestCase(Seq("1", "2", "3"), Right(Seq(1, 2, 3))),
    TraverseTestCase(Seq(""), Left("decoding error")),
    TraverseTestCase(Seq.empty[String], Right(Seq.empty[Int])),
    TraverseTestCase(Seq("1", "2.s", "3"), Left("decoding error"))
  )

  case class SequenceTestCase(
      input: Seq[Either[String, Int]],
      output: Either[String, Seq[Int]]
  ) {
    override def toString: String = s"Sequence: $input => $output"
  }

  val sequenceCases: Seq[SequenceTestCase] = Seq(
    SequenceTestCase(Seq(Right(1), Right(2), Right(3)), Right(Seq(1, 2, 3))),
    SequenceTestCase(Seq(Left("err")), Left("err")),
    SequenceTestCase(Seq.empty[Either[String, Int]], Right(Seq.empty[Int])),
    SequenceTestCase(
      Seq(Right(1), Left("invalid"), Right(3)),
      Left("invalid")
    )
  )

  sequenceCases.foreach { t =>
    test(t.toString) {
      assertEquals(E007EitherSequenceTraverse.sequence(t.input), t.output)
    }
  }

  traverseCases.foreach { t =>
    test(t.toString) {
      assertEquals(
        E007EitherSequenceTraverse.traverse(t.input)(parseInt),
        t.output
      )
    }
  }
}

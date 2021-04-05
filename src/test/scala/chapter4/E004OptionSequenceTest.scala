package chapter4

import munit.FunSuite

class E004OptionSequenceTest extends FunSuite {
  case class TestCase(
      input: Seq[Option[Int]],
      output: Option[Seq[Int]]
  ) {
    override def toString: String = s"$input => $output"
  }

  val cases = Seq(
    TestCase(Seq(Some(1), Some(2)), Some(Seq(1, 2))),
    TestCase(Seq(None, Some(2)), None),
    TestCase(Seq(Some(1), None), None),
    TestCase(Seq(None, None), None)
  )

  cases.foreach { t =>
    test(t.toString) {
      assertEquals(E004OptionSequence.sequence(t.input), t.output)
    }
  }
}

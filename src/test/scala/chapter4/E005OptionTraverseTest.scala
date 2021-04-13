package chapter4

import munit.FunSuite

class E005OptionTraverseTest extends FunSuite {
  case class TestCase(
      input: Seq[String],
      output: Option[Seq[Int]]
  ) {
    override def toString: String = s"$input => $output"
  }

  val parseInt: String => Option[Int] = _.toIntOption

  val cases = Seq(
    TestCase(Seq("1", "2", "3"), Some(Seq(1, 2, 3))),
    TestCase(Seq(""), None),
    TestCase(Seq.empty[String], Some(Seq.empty[Int])),
    TestCase(Seq("1", "2.s", "3"), None)
  )

  cases.foreach { t =>
    test(t.toString) {
      assertEquals(E005OptionTraverse.traverse(t.input)(parseInt), t.output)
    }
  }
}

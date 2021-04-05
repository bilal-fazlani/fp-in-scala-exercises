package chapter4

import munit.FunSuite

class E003Map2Test extends FunSuite {
  case class TestCase(
      first: Option[Int],
      second: Option[Int],
      expected: Option[Int]
  ) {
    override def toString: String = s"$first,$second => $expected"
  }

  val f: (Int, Int) => Int = _ + _
  val cases = Seq(
    TestCase(None, None, None),
    TestCase(None, Some(1), None),
    TestCase(Some(2), None, None),
    TestCase(Some(4), Some(1), Some(5))
  )

  cases.foreach { t =>
    test(t.toString) {
      assertEquals(E003Map2.map2(t.first, t.second)(f), t.expected)
    }
  }
}

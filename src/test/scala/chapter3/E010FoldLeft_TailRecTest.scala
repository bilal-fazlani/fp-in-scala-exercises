package chapter3

import chapter3.E010FoldLeft_TailRec._
import munit.FunSuite

class E010FoldLeft_TailRecTest extends FunSuite {
  test("foldLeft") {
    val result = List.foldLeft(List("A", "B", "C"), "")(_ + " " + _)
    assertNoDiff(result, "C B A")
  }
}

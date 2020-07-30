package chapter3

import chapter3.E009Length._
import munit.FunSuite

class E009LengthTest extends FunSuite {
  test("length of Nil should be 0") {
    assertEquals(List.length(Nil), 0)
  }

  test("length of list") {
    assertEquals(List.length(List(1, 4)), 2)
  }
}

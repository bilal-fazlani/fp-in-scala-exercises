package chapter3

import chapter3.E006Init._
import munit.FunSuite

class E006InitTest extends FunSuite {
  test("init for a Nil should be Nil") {
    assertEquals(List.init(Nil), Nil)
  }

  test("init for a single value list should be Nil") {
    assertEquals(List.init(List(1)), Nil)
  }

  test("init for a 2 value list should be single value list") {
    assertEquals(List.init(List(1, 2)), List(1))
  }

  test("init for a multi value list") {
    assertEquals(List.init(List(1, 2, 3)), List(1, 2))
  }
}

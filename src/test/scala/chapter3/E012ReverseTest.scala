package chapter3

import chapter3.E012Reverse._
import munit.FunSuite

class E012ReverseTest extends FunSuite {
  test("reverse of Nil is Nil") {
    assertEquals(List.reverse(Nil), Nil)
  }

  test("reverse of single item") {
    assertEquals(List.reverse(List("a")), List("a"))
  }

  test("reverse of multiple items") {
    assertEquals(List.reverse(List("a", "b")), List("b", "a"))
  }
}

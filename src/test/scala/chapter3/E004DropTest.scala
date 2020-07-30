package chapter3

import chapter3.E004Drop._
import munit.FunSuite

class E004DropTest extends FunSuite {
  test("drop Nil should be Nil") {
    assertEquals(List.drop(Nil, 3), Nil)
  }

  test("drop n items") {
    assertEquals(List.drop(List(1, 2, 3, 4, 5), 2), List(3, 4, 5))
  }

  test("drop all items when n is larger than list") {
    assertEquals(List.drop(List(1, 2, 3, 4, 5), 7), Nil)
  }

  test("drop all items when n is equal to list") {
    assertEquals(List.drop(List(1, 2), 2), Nil)
  }
}

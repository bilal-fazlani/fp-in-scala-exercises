package chapter3

import chapter3.E003SetHead._
import munit.FunSuite

class E003SetHeadTest extends FunSuite {
  test("set head for Nil should be Nil") {
    assertEquals(List.setHead(Nil, 3), Nil)
  }

  test("set head for List") {
    assertEquals(List.setHead(List(3, 3), 4), List(4, 3))
  }
}

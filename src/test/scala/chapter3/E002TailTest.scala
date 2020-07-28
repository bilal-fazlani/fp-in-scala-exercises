package chapter3

import chapter3.E002Tail._
import munit.FunSuite

class E002TailTest extends FunSuite {
  test("tail of Nil should be Nil") {
    assertEquals(List.tail(Nil), Nil)
  }
  
  test("tail of list with Nil tail") {
    assertEquals(List.tail(List(3)), Nil)
  }

  test("tail of list with values") {
    assertEquals(List.tail(List(3,4,2)), List(4,2))
  }
}

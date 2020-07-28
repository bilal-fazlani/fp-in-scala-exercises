package chapter3

import chapter3.E005DropWhile._
import munit.FunSuite

class E005DropWhileTest extends FunSuite {
  test("dropWhile Nil should be Nil") {
    assertEquals(List.dropWhile(Nil, _ !=2), Nil)
  }

  test("dropWhile not matched should drop all elements") {
    assertEquals(List.dropWhile(List(3,5,6,7), _ != 2), Nil)
  }
}

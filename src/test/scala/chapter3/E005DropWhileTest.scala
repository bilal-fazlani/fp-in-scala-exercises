package chapter3

import chapter3.E005DropWhile._
import munit.FunSuite

class E005DropWhileTest extends FunSuite {
  test("dropWhile Nil should be Nil") {
    assertEquals(List.dropWhile[Int](Nil, _ != 2), Nil)
  }

  test("dropWhile drop until predicate matches") {
    assertEquals(List.dropWhile[Int](List(3, 5, 6, 7), _ != 5), List(5, 6, 7))
  }

  test("dropWhile drop until predicate matches - repeat value") {
    assertEquals(
      List.dropWhile[Int](List(3, 5, 6, 7, 5, 8), _ != 5),
      List(5, 6, 7, 5, 8)
    )
  }

  test("dropWhile drop all value if predicate does not match for any value") {
    assertEquals(List.dropWhile[Int](List(3, 5, 6, 7), _ != 2), Nil)
  }

  test("dropWhile return same list if first element matches predicate") {
    assertEquals(
      List.dropWhile[Int](List(3, 5, 6, 7), _ != 3),
      List(3, 5, 6, 7)
    )
  }
}

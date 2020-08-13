package chapter3

import chapter3.E007FoldRightSumProduct._
import munit.FunSuite

class E007FoldRightSumProductTest extends FunSuite {
  test("foldRight") {
    val list   = List("A", "B", "C")
    val result = List.foldRight(list, "")(_ + " " + _)
    assertNoDiff(result, "A B C")
  }

  test("sum for a Nil should be 0") {
    assertEquals(List.sum(Nil), 0)
  }

  test("product for a Nil should be 1") {
    assertEquals(List.product(Nil), 1)
  }

  test("sum of list") {
    assertEquals(List.sum(List(1, 2)), 3)
  }

  test("product with zero should be zero") {
    assertEquals(List.product(List(1, 0, 4)), 0)
  }

  test("product of list") {
    assertEquals(List.product(List(1, 2, 4)), 8)
  }
}

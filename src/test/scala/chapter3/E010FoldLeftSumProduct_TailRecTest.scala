package chapter3

import chapter3.E010FoldLeftSumProduct_TailRec._
import munit.FunSuite

class E010FoldLeftSumProduct_TailRecTest extends FunSuite {
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

  test("length of Nil should be 0") {
    assertEquals(List.length(Nil), 0)
  }

  test("length of list") {
    assertEquals(List.length(List(1, 4)), 2)
  }
}

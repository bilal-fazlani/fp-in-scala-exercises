package chapter3

import munit.FunSuite
import E001SumProduct._

class E001SumProductTest extends FunSuite {
  test("sum of Nil should be 0") {
    assertEquals(List.sum(Nil), 0)
  }

  test("sum of values") {
    assertEquals(List.sum(Cons(3,Cons(6, Nil))), 9)
  }

  test("product of Nil should be 1") {
    assertEquals(List.product(Nil), 1)
  }

  test("product with 0 should be 0") {
    assertEquals(List.product(Cons(4,Cons(0, Cons(4, Nil)))), 0)
  }

  test("product of values") {
    assertEquals(List.product(Cons(4,Cons(2, Cons(4, Nil)))), 32)
  }
}

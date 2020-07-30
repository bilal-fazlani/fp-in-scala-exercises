package chapter3

import chapter3.E0051Append._
import munit.FunSuite

class E0051AppendTest extends FunSuite {
  test("append for a Nil list1 should be list2") {
    assertEquals(List.append(Nil, List(1)), List(1))
  }

  test("append for a list1 should be list1 + list2") {
    assertEquals(List.append(List(1, 2), List(3, 4)), List(1, 2, 3, 4))
  }

  test("append for a list1 and elem should be list1 + elem") {
    assertEquals(List.append(List(1, 2), 4), List(1, 2, 4))
  }
}

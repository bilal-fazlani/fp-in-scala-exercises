package chapter4

import munit.FunSuite
import chapter4.E001Option._

class E001OptionTest extends FunSuite {
  test("map should return None for None") {
    val none = None
    assertEquals(none.map(_ => 4), None)
  }

  test("map should return map a some object") {
    val some = Some(3)
    assertEquals(some.map(x => (x * 2).toString), Some("6"))
  }

  test("flatMap should return None for initial None") {
    val none = None
    assertEquals(none.flatMap(_ => Some(4)), None)
  }

  test("flatMap should return None for next None") {
    val some = Some(4)
    assertEquals(some.flatMap(i => None), None)
  }

  test("flatMap should return Some for both Some") {
    val some = Some(4)
    assertEquals(some.flatMap(i => Some(i + 1)), Some(5))
  }
}

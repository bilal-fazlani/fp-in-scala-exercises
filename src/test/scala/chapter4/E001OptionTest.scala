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
}

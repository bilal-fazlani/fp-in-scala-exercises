package chapter4

import munit.FunSuite
import chapter4.E001Option._

class E001OptionTest extends FunSuite {
  test("map should return None for None") {
    val none = None
    assertEquals(none.map(_ => 4), None)
  }

  test("map should return new value for Some ") {
    val some = Some(3)
    assertEquals(some.map(x => (x * 2).toString), Some("6"))
  }

  test("flatMap should return None for None-Some") {
    val none = None
    assertEquals(none.flatMap(_ => Some(4)), None)
  }

  test("flatMap should return None for None-None") {
    val some = Some(4)
    assertEquals(some.flatMap(i => None), None)
  }

  test("flatMap should return Some for Some-Some") {
    val some = Some(4)
    assertEquals(some.flatMap(i => Some(i + 1)), Some(5))
  }

  test("flatMap should return None for Some-None") {
    val some = Some(4)
    assertEquals(some.flatMap(i => None), None)
  }

  test("filter None-false = None") {
    val none: Option[Int] = None
    assertEquals(none.filter(_ > 0), None)
  }

  test("filter Some-true = Some") {
    val some: Option[Int] = Some(3)
    assertEquals(some.filter(_ > 0), Some(3))
  }

  test("filter Some-false = None") {
    val none: Option[Int] = Some(3)
    assertEquals(none.filter(_ < 0), None)
  }

  test("getOrElse None = new value") {
    val none: Option[Int] = None
    assertEquals(none.getOrElse(3), 3)
  }

  test("getOrElse Some = previous value") {
    val some: Option[Int] = Some(3)
    assertEquals(some.getOrElse(5), 3)
  }

  test("orElse Some A - Some B = Some A") {
    val some: Option[Int] = Some(3)
    assertEquals(some.orElse(Some(5)), Some(3))
  }

  test("orElse Some A - None = Some A") {
    val some: Option[Int] = Some(3)
    assertEquals(some.orElse(None), Some(3))
  }

  test("orElse None - Some B = Some B") {
    val none: Option[Int] = None
    assertEquals(none.orElse(Some(5)), Some(5))
  }

  test("orElse None - None = None") {
    val none: Option[Int] = None
    assertEquals(none.orElse(None), None)
  }
}

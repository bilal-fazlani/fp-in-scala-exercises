package chapter4

import munit.FunSuite
import E006Either._

class E006EitherTest extends FunSuite {
  //region map
  test("map should return Left for Left") {
    val current: Either[String, Int] = Left("error1")
    val actual                       = current.map(_ + 1)
    assertEquals(actual, Left("error1"))
  }
  test("map should return Right with new value for a Right") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.map(_ + 1)
    assertEquals(actual, Right(2))
  }
  //endregion

  //region flatMap
  test("flatMap should return Left for previous Left") {
    val current: Either[String, Int] = Left("error1")
    val actual                       = current.flatMap(i => Right(i + 1))
    assertEquals(actual, Left("error1"))
  }
  test("flatMap should return Left for new Left") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.flatMap(i => Left("error"))
    assertEquals(actual, Left("error"))
  }
  test("flatMap should return Right with new value - happy case") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.flatMap(i => Right(i + 1))
    assertEquals(actual, Right(2))
  }
  //endregion

  //region orElse
  test("orElse should return previous Right for new Right") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.orElse(Right(2))
    assertEquals(actual, Right(1))
  }
  test("orElse should return previous Right for new Left") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.orElse(Left("error"))
    assertEquals(actual, Right(1))
  }
  test("orElse should return new Left for new Left") {
    val current: Either[String, Int] = Left("fatal")
    val actual                       = current.orElse(Left("error"))
    assertEquals(actual, Left("error"))
  }
  test("orElse should return new Right for new Right") {
    val current: Either[String, Int] = Left("fatal")
    val actual                       = current.orElse(Right(1))
    assertEquals(actual, Right(1))
  }
  //endregion

  //region map2
  test("map2 should return Right when both are right") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.map2(Right(2))(_ + _)
    assertEquals(actual, Right(3))
  }
  test("map2 should return Left when current is Left") {
    val current: Either[String, Int] = Left("error")
    val actual                       = current.map2(Right(2))(_ + _)
    assertEquals(actual, Left("error"))
  }
  test("map2 should return Left when next is Left") {
    val current: Either[String, Int] = Right(1)
    val actual                       = current.map2[String, Int, Int](Left("error"))(_ + _)
    assertEquals(actual, Left("error"))
  }
  test("map2 should return Left both are Left") {
    val current: Either[String, Int] = Left("error")
    val actual                       = current.map2[String, Int, Int](Left("fatal"))(_ + _)
    assertEquals(actual, Left("error"))
  }
  //endregion
}

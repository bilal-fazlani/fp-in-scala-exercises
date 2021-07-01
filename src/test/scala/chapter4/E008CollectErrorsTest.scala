package chapter4

import munit.FunSuite
import E008CollectErrors._

object Example {
  case class Name(value: String)
  case class Age(value: Int)
  case class Person(name: Name, age: Age)

  def makeName(value: String): Validated[String, Name] =
    if (value.nonEmpty && !value.contains("#")) Valid(Name(value))
    else Invalid("Invalid Name")

  def makeAge(value: Int): Validated[String, Age] =
    if (value < 0 || value > 200) Invalid("Invalid Age")
    else Valid(Age(value))

  def mkPerson(name: String, age: Int): Validated[String, Person] =
    makeName(name).map2(makeAge(age)) {
      case (name, age) => Person(name, age)
    }
}

class E008CollectErrorsTest extends FunSuite {
  import Example._
  test("should combine multiple invalids") {
    assertEquals(
      mkPerson("#bilal", -8),
      Invalid(
        Seq(
          "Invalid Name",
          "Invalid Age"
        )
      )
    )
  }

  test("invalid should invalidate previous valid") {
    assertEquals(
      mkPerson("bilal", -8),
      Invalid(
        Seq(
          "Invalid Age"
        )
      )
    )
  }

  test("previous invalid should ignore new valid") {
    assertEquals(
      mkPerson("#bilal", 8),
      Invalid(
        Seq(
          "Invalid Name"
        )
      )
    )
  }

  test("valid value") {
    assertEquals(
      mkPerson("bilal", 20),
      Valid(Person(Name("bilal"), Age(20)))
    )
  }
}

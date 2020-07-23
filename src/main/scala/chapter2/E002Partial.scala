package chapter2

object E002Partial {

  def main(args: Array[String]): Unit = {
    trait A
    trait B
    trait C

    def partial1(a: A, f: ((A, B) => C)): B => C =
      f(a, _)

    def partial1_alt(a: A, f: ((A, B) => C)): B => C =
      b => f(a, b)

    def partial1_alt2(a: A, f: ((A, B) => C)): B => C =
      (b: B) => f(a, b)
  }

}

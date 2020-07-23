package chapter2

object E004Uncurry {

  def main(args: Array[String]): Unit = {
    trait A
    trait B
    trait C

    def uncurry(f: A => (B => C)): (A, B) => C =
      (a, b) => f(a)(b)
  }

}

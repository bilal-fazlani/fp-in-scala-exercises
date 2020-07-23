package chapter2

object E003Curry {

  def main(args: Array[String]): Unit = {
    trait A
    trait B
    trait C

    def curry(f: (A, B) => C): A => (B => C) =
      a => (b => f(a, b))
  }

}

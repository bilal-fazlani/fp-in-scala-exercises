package chapter2

object E005Compose {

  def main(args: Array[String]): Unit = {
    trait A
    trait B
    trait C

    def componse(f: A => B, g: B => C): A => C =
      a => g(f(a))
  }

}

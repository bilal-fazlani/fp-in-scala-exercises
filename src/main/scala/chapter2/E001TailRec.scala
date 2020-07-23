package chapter2

import scala.annotation.tailrec

object E001TailRec {

  def main(args: Array[String]): Unit = {

    def fib(number: Int): Int =
      number match {
        case 1 => 0
        case 2 => 1
        case x => fib(x - 1) + fib(x - 2)
      }

    @tailrec
    def fib2(number: Int, current: Int = 0, prev: Int = 1): Int = {
      if (number == 1) current
      else fib2(number - 1, current + prev, current)
    }

    Range(1, 15).foreach(x => println(fib2(x)))

  }

}

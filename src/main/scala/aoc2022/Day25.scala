package aoc2022

import scala.collection.mutable

class Day25 extends aoc.Day(2022, 25) {
  override def part1(input: Array[String]): Any = {
    decimalToSnafu(input.map(snafuToDecimal).sum)
  }

  private[this] val test = """1=-0-2
                             |12111
                             |2=0=
                             |21
                             |2=01
                             |111
                             |20012
                             |112
                             |1=-1=
                             |1-12
                             |12
                             |1=
                             |122""".stripMargin.split("\n")

  private[this] val chars = Array('=', '-', '0', '1', '2')
  private[this] def snafuToDecimal(input: String): Long = {
    var radix = 0
    var place = 1L
    var n = 0L
    while (radix < input.length) {
      n = n + place * (chars.indexOf(input(input.length - 1 - radix)) - 2)
      place = place * 5L
      radix = radix + 1
    }
    n
  }

  private[this] def decimalToSnafu(input: Long): String = {
    var n = 1L
    while (n <= input) {
      n *= 5
    }
    var numLeft = input
    var output = mutable.Buffer[Int]()
    while (n >= 5) {
      n /= 5
      val place = numLeft / n
      numLeft = numLeft - (place * n)
      output.append(place.toInt)
    }
    output = output.reverse
    var result = ""
    var n2 = 0
    while (n2 < output.size) {
      if (output(n2) == 3) {
        if (n2 == output.size - 1) {
          output.append(1)
        } else {
          output(n2 + 1) = output(n2 + 1) + 1
        }
        result = "=" + result
      } else if (output(n2) == 4) {
        if (n2 == output.size - 1) {
          output.append(1)
        } else {
          output(n2 + 1) = output(n2 + 1) + 1
        }
        result = "-" + result
      } else {
        result = s"${output(n2)}$result"
      }
      n2 = n2 + 1
    }
    result
  }

  override def part2(input: Array[String]): Any = "Merry Christmas"
}

object Day25 {
  def apply() = new Day25
}
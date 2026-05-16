package aoc2022

import aoc.NewDay

import scala.collection.mutable

class Day25 extends NewDay(2022, 25) {
  part(1) {
    execute { in =>
      decimalToSnafu(in.map(snafuToDecimal).sum)
    }
  }

  part(2) {
    execute { _ =>
      "Merry Christmas"
    }
  }

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
      output.prepend(place.toInt)
    }
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
}

object Day25Main extends Day25

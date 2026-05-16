package aoc2020

import aoc.NewDay
import aoc2020.Day5.{binaryToInt, sumFrom, toBinary}

class Day5 extends NewDay(2020, 5) {
  part(1) {
    execute { in =>
      "The highest SeatID is " + in.map(toBinary).map(binaryToInt).max
    }
  }

  part(2) {
    execute { in =>
      val sorted = in.map(toBinary).map(binaryToInt).sorted
      "The missing SeatID is " + (sumFrom(sorted.head, sorted.last) - sorted.sum)
    }
  }
}

object Day5Main extends Day5

object Day5 {
  def toBinary(line: String): String = {
    line.replace('F', '0')
      .replace('B', '1')
      .replace('R', '1')
      .replace('L', '0')
  }

  def binaryToInt(binaryLine: String): Int = {
    Integer.parseInt(binaryLine, 2)
  }

  def sumFrom(low: Int, high: Int): Int = {
    (low + high) * (high + 1 - low) / 2
  }
}

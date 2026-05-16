package aoc2015

import aoc.NewDay
import Day25._

import scala.util.matching.Regex

class Day25 extends NewDay(2015, 25) {
  part(1) {
    execute { in =>
      val (r, c) = in(0) match {
        case Code(row, column) =>
          (row.toInt, column.toInt)
      }

      var currentRow = 1
      var currentCol = 1
      var currentCode: Long = 20151125
      while (!(currentCol == c && currentRow == r)) {
        if (currentRow == 1) {
          currentRow = currentCol + 1
          currentCol = 1
        } else {
          currentRow = currentRow - 1
          currentCol = currentCol + 1
        }
        currentCode = (currentCode * 252533) % 33554393
      }
      currentCode.toString
    }
  }

  part(2) {
    execute { _ => "" }
  }
}

object Day25 {
  val Code: Regex = raw"To continue, please consult the code grid in the manual.  Enter the code at row (\d+), column (\d+).".r
}

object Day25Main extends Day25

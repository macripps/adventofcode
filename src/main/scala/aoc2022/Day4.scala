package aoc2022

import aoc.NewDay

class Day4 extends NewDay(2022, 4) {
  part(1) {
    execute { in =>
      in.map(toRanges).count {
        case (leftMin, leftMax, rightMin, rightMax) =>
          (leftMin <= rightMin && leftMax >= rightMax) || (rightMin <= leftMin && rightMax >= leftMax)
      }
    }
  }

  part(2) {
    execute { in =>
      in.map(toRanges).count {
        case (leftMin, leftMax, rightMin, rightMax) =>
          leftMin <= rightMax && leftMax >= rightMin
      }
    }
  }

  private[this] def toRanges(line: String): (Int, Int, Int, Int) = {
    val assignments = line.split(',')
    val left = assignments(0).split('-')
    val right = assignments(1).split('-')
    (left(0).toInt, left(1).toInt, right(0).toInt, right(1).toInt)
  }
}

object Day4Main extends Day4

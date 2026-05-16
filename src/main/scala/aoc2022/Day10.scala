package aoc2022

import aoc.NewDay

import Day10._

class Day10 extends NewDay(2022, 10) {
  part(1) {
    execute { in =>
      var X = 1L
      var cycleValues = Seq[Long](0)
      in.foreach {
        case addx(amt) =>
          cycleValues = cycleValues :+ X :+ X
          X = X + amt.toLong
        case _@noop =>
          cycleValues = cycleValues :+ X
      }
      (20 * cycleValues(20)) +
        (60 * cycleValues(60)) +
        (100 * cycleValues(100)) +
        (140 * cycleValues(140)) +
        (180 * cycleValues(180)) +
        (220 * cycleValues(220))
    }
  }

  part(2) {
    execute { in =>
      var spritePosition = 1
      var cycle = 1
      var position = 0
      in.foreach { line =>
        line match {
          case addx(amt) =>
            if (math.abs(spritePosition - position) <= 1) {
              print('#')
            } else {
              print(' ')
            }
            if (cycle == 40) {
              println()
              cycle = 0
              position = -1
            }
            cycle = cycle + 1
            position = position + 1
            if (math.abs(spritePosition - position) <= 1) {
              print('#')
            } else {
              print(' ')
            }
            spritePosition = spritePosition + amt.toInt
          case _@noop =>
            if (math.abs(spritePosition - position) <= 1) {
              print('#')
            } else {
              print(' ')
            }
        }
        if (cycle == 40) {
          println()
          cycle = 0
          position = -1
        }
        cycle = cycle + 1
        position = position + 1
      }
      ""
    }
  }
}

object Day10Main extends Day10

object Day10 {
  val noop = raw"noop".r
  val addx = raw"addx (-?\d+)".r
}

package aoc2018

import aoc.Day

class Day11 extends Day(2018, 11) {
  override def part1: String = {
    val input = 3999
    val grid = Array.ofDim[Int](300, 300)

    (1 to 300).foreach { y =>
      (1 to 300).foreach { x=>
        grid(y-1)(x-1) = powerLevel(x, y, input)
      }
    }
    var maxScore = Int.MinValue
    var maxX = -1
    var maxY = -1
    (0 to 297).foreach { y =>
      (0 to 297).foreach { x =>
        val score = grid(y)(x) + grid(y)(x+1) + grid(y)(x+2) +
          grid(y+1)(x) + grid(y+1)(x+1) + grid(y+1)(x+2) +
          grid(y+2)(x) + grid(y+2)(x+1) + grid(y+2)(x+2)
        if (score > maxScore) {
          maxScore = score
          maxX = x
          maxY = y
        }
      }
    }
    "%d,%d".format(maxX + 1, maxY + 1)
  }

  def powerLevel(x: Int, y: Int, serialNumber: Int): Int = {
    val rackId = x + 10
    var powerLevel = y * rackId
    powerLevel = powerLevel + serialNumber
    powerLevel = powerLevel * rackId
    var hD = (powerLevel % 1000) / 100
    hD = hD - 5
    hD
  }

  override def part2: String = {
    val input = 3999
    val grid = Array.ofDim[Int](300, 300)

    (1 to 300).foreach { y =>
      (1 to 300).foreach { x=>
        grid(y-1)(x-1) = powerLevel(x, y, input)
      }
    }

    var maxScore = Int.MinValue
    var maxX = -1
    var maxY = -1
    var gridSize = -1
    (1 to 299).foreach { size =>
      (0 to 300 - size).foreach { y =>
        (0 to 300 - size).foreach { x =>
          var score = 0
          (x until x + size).foreach {x_ =>
            (y until y+size).foreach { y_ =>
              score = score + grid(y_)(x_)
            }
          }
          if (score > maxScore) {
            maxScore = score
            maxX = x
            maxY = y
            gridSize = size
          }
        }
      }
    }
    "%d,%d,%d".format(maxX + 1, maxY + 1, gridSize)
  }
}

object Day11 {
  def apply() = new Day11()
}

package aoc2021

import aoc.NewDay

class Day7 extends NewDay(2021, 7) {
  part(1) {
    execute { in =>
      val positions = in.head.split(',').map(_.toInt)
      (positions.min to positions.max).map { p =>
        positions.map(pos => cost1(p - pos)).sum
      }.min.toString
    }
  }

  def cost1(a: Int): Int = Math.abs(a)

  part(2) {
    execute { in =>
      val positions = in.head.split(',').map(_.toInt)
      (positions.min to positions.max).map { p =>
        positions.map(pos => cost2(p - pos)).sum
      }.min.toString
    }
  }

  def cost2(a: Int): Int = {
    val b = Math.abs(a)
    (b * (b+1)) / 2
  }
}

object Day7Main extends Day7

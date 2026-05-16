package aoc2017

import aoc.NewDay

class Day13 extends NewDay(2017, 13) {
  part(1) {
    execute { in =>
      in.map { line =>
        val kv = line.split(": ")
        val col = kv(0).toInt
        val depth = kv(1).toInt
        if (pos(col, depth) == 1) {
          col * depth
        } else 0
      }.sum.toString
    }
  }

  def pos(col: Int, depth: Int): Int = {
    val r = (1 to depth) ++ Range.inclusive(depth - 1, 2, -1)
    r(col % r.length)
  }

  part(2) {
    execute { in =>
      LazyList.from(1).find { x =>
        val sc = in.map { line =>
          val kv = line.split(": ")
          val col = kv(0).toInt
          val depth = kv(1).toInt
          if (pos(col + x, depth) == 1) {
            1
          } else 0
        }.sum
        sc == 0
      }.get.toString
    }
  }
}

object Day13Main extends Day13

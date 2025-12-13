package aoc2025

import aoc.NewDay

class Day12 extends NewDay(2025, 12) {
  part(1) {
    execute { ls =>
      val sizes = Array(7, 7, 6, 7, 5, 7)
      ls.count { l =>
        val Array(size, pieces) = l.split(": ")
        val Array(x,y) = size.split('x').map(_.toInt)
        val pieceCounts = pieces.split(' ').map(_.toInt)
        val regionArea = x * y
        val pieceArea = pieceCounts.zipWithIndex.map { case (count, idx) => count * sizes(idx) }.sum
        regionArea >= pieceArea
      }
    }
  }
}

object Day12Main extends Day12

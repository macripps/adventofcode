package aoc2022

import aoc.NewDay

class Day6 extends NewDay(2022, 6) {
  part(1) {
    execute { in =>
      val line = in.head
      (3 until line.length).find { n =>
        line.slice(n-3, n + 1).toSet.size == 4
      }.get+1
    }
  }

  part(2) {
    execute { in =>
      val line = in.head
      (13 until line.length).find { n =>
        line.slice(n - 13, n + 1).toSet.size == 14
      }.get+1
    }
  }
}

object Day6Main extends Day6

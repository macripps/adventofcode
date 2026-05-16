package aoc2019

import aoc.NewDay

class Day1 extends NewDay(2019, 1) {
  part(1) {
    execute { in =>
      in.map(_.toInt / 3 - 2).sum
    }
  }

  part(2) {
    execute { in =>
      in.map(s => fuel(s.toInt)).sum
    }
  }

  def fuel(f: Int): Int = {
    if (f <= 0) 0 else {
      val m = f / 3 - 2
      if (m < 0) 0 else m + fuel(m)
    }
  }
}

object Day1Main extends Day1

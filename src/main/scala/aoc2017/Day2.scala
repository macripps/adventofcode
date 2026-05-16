package aoc2017

import aoc.NewDay

class Day2 extends NewDay(2017, 2) {
  part(1) {
    execute { in =>
      in.map { x =>
        val y = x.split(raw"\s+").map(x => x.toInt)
        y.max - y.min
      }.sum.toString
    }
  }

  part(2) {
    execute { in =>
      in.map { x =>
        x.split(raw"\s+").map(x => x.toInt).sorted.combinations(2).filter(l => l(1)%l(0) == 0).map{z => z(1)/z(0)}.next()
      }.sum.toString
    }
  }
}

object Day2Main extends Day2

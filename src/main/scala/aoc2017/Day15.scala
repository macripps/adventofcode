package aoc2017

import aoc.NewDay

class Day15 extends NewDay(2017, 15) {
  part(1) {
    execute { _ =>
      var a = 618L
      var b = 814L
      (1 to 40_000_000).map { _ =>
        a = (a * 16807) % Int.MaxValue
        b = (b * 48271) % Int.MaxValue
        if ((a & 0xffff) == (b & 0xffff)) 1 else 0
      }.sum.toString
    }
  }

  part(2) {
    execute { _ =>
      var a = 618L
      var b = 814L
      (1 to 5_000_000).map { _ =>
        a = (a * 16807) % Int.MaxValue
        while (a % 4 != 0) {
          a = (a * 16807) % Int.MaxValue
        }
        b = (b * 48271) % Int.MaxValue
        while (b % 8 != 0) {
          b = (b * 48271) % Int.MaxValue
        }
        if ((a & 0xffff) == (b & 0xffff)) 1 else 0
      }.sum.toString
    }
  }
}

object Day15Main extends Day15

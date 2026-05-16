package aoc2015

import aoc.NewDay

class Day20 extends NewDay(2015, 20) {
  part(1) {
    execute { in =>
      val target = in.head.toInt

      val houses = Array.ofDim[Int](1_000_000)
      (1 until houses.length).foreach { elf =>
        Range(elf, houses.length-1, elf).foreach { step =>
          houses(step) = houses(step) + (elf * 10)
        }
      }
      houses.indexWhere(k => k >= target).toString
    }
  }

  part(2) {
    execute { in =>
      val target = in.head.toInt

      val houses = Array.ofDim[Int](1_000_000)
      (1 until houses.length).foreach { elf =>
        Range(elf, math.min(50 * elf, houses.length-1), elf).foreach { step =>
          houses(step) = houses(step) + (elf * 11)
        }
      }
      houses.indexWhere(k => k >= target).toString
    }
  }
}

object Day20Main extends Day20

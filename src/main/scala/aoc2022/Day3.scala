package aoc2022

import aoc.NewDay

class Day3 extends NewDay(2022, 3) {
  part(1) {
    execute { in =>
      in.map { line =>
        val (left, right) = line.splitAt(line.length/2)
        left.filter {
          l: Char => right.contains(l)
        }.toSet.map(toPriority).sum
      }.sum.toString
    }
  }

  part(2) {
    execute { in =>
      in.grouped(3).map { lines =>
        lines.head.filter { c =>
          lines.tail.forall(l => l.contains(c))
        }.toSet.map(toPriority).sum
      }.sum.toString
    }
  }

  private[this] def toPriority(p: Char): Int = {
    if (p >= 'a' && p <= 'z') {
      p - '`'
    } else if (p >= 'A' && p <= 'Z') {
      p + 26 - '@'
    } else 0
  }
}

object Day3Main extends Day3

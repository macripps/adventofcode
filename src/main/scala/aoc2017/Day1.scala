package aoc2017

import aoc.NewDay

class Day1 extends NewDay(2017, 1) {
  part(1) {
    execute { in =>
      val line = in.head
      line.indices.filter { x => line(x) == line((x + 1) % line.length)}.map(x => (line(x) - '0').toLong).sum.toString
    }
  }

  part(2) {
    execute { in =>
      val line = in.head
      line.indices.filter { x => line(x) == line((x +(line.length/2)) % line.length)}.map(x => (line(x) - '0').toLong).sum.toString
    }
  }
}

object Day1Main extends Day1

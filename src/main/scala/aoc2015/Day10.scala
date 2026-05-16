package aoc2015

import aoc.NewDay
import Day10._

class Day10 extends NewDay(2015, 10) {
  part(1) {
    execute { in =>
      var line = in(0)
      (1 to 40).foreach { i =>
        line = next(line)
      }

      line.length.toString
    }
  }

  part(2) {
    execute { in =>
      var line = in(0)
      (1 to 50).foreach { i =>
        line = next(line)
      }

      line.length.toString
    }
  }
}

object Day10 {
  def next(line: String): String = {
    var i = 0
    val out = new StringBuilder
    while (i < line.length) {
      var j = i + 1
      while (j < line.length && line(i) == line(j)) {
        j = j + 1
      }
      out.append((j - i).toString).append(line(i))
      i = j
    }
    out.toString()
  }
}

object Day10Main extends Day10

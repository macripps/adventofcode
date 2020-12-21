package aoc2015

import aoc.Day
import Day10._

class Day10 extends Day(2015, 10) {
  override def part1: String = {
    var line = input(0)
    (1 to 40).foreach { i =>
      line = next(line)
    }

    line.length.toString
  }

  override def part2: String = {
    var line = input(0)
    (1 to 50).foreach { i =>
      line = next(line)
    }

    line.length.toString
  }
}

object Day10 {
  def apply() = new Day10()

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

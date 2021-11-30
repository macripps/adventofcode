package aoc2017

import aoc.Day

class Day5 extends Day(2017, 5) {
  override def part1(input: Array[String]): String = {
    val prog = input.map(_.toInt)
    var steps = 0
    var ep = 0
    while (ep >= 0 && ep < prog.length) {
      val offset = prog(ep)
      prog(ep) = offset + 1
      ep = ep + offset
      steps = steps + 1
    }
    steps.toString
  }

  override def part2(input: Array[String]): String = {
    val prog = input.map(_.toInt)
    var steps = 0
    var ep = 0
    while (ep >= 0 && ep < prog.length) {
      val offset = prog(ep)
      prog(ep) = if (offset >= 3) offset - 1 else offset + 1
      ep = ep + offset
      steps = steps + 1
    }
    steps.toString
  }
}

object Day5 {
  def apply() = new Day5()
}

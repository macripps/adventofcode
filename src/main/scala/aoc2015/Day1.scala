package aoc2015

import aoc.Day

class Day1 extends Day(2015, 1) {

  override def part1: String = {
    "The final floor is " + input(0).map[Int](c => if (c == '(') 1 else -1).sum
  }

  override def part2: String = {
    val s = input(0).map[Int](c => if (c == '(') 1 else -1)
    val pos = s.indices.find(i => s.take(i).sum < 0).get
    "" + pos
  }
}

object Day1 {
  def apply() = new Day1()
}

package aoc2017

import aoc.Day

class Day1 extends Day(2017, 1) {
  override def part1: String = {
    val line = input.head
    line.indices.filter { x => line(x) == line((x + 1) % line.length)}.map(x => (line(x) - '0').toLong).sum.toString
  }

  override def part2: String = {
    val line = input.head
    line.indices.filter { x => line(x) == line((x +(line.length/2)) % line.length)}.map(x => (line(x) - '0').toLong).sum.toString
  }
}

object Day1 {
  def apply() = new Day1()
}

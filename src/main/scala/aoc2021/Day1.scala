package aoc2021

import aoc.Day

class Day1 extends Day(2021, 1) {
  override def part1(input: Array[String]): String = {
    input.zip(input.tail).count { case (l: String, r: String) =>
      r.toInt > l.toInt
    }.toString
  }

  override def part2(input: Array[String]): String = {
    input.zip(input.drop(3)).count { case (l: String, r: String) =>
      r.toInt > l.toInt
    }.toString
  }
}

object Day1 {
  def apply() = new Day1
}

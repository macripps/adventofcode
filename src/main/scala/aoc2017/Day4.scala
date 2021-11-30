package aoc2017

import aoc.Day

class Day4 extends Day(2017, 4) {
  override def part1(input: Array[String]): String = {
    input.map { l =>
      val x = l.split(" ")
      x.toSet.size == x.length
    }.count(x => x).toString
  }

  override def part2(input: Array[String]): String = {
    input.map { l =>
      val x = l.split(" ").map(_.toCharArray.sorted.mkString)
      x.toSet.size == x.length
    }.count(x => x).toString
  }

}

object Day4 {
  def apply() = new Day4()
}

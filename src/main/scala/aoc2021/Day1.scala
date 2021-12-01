package aoc2021

import aoc.Day

class Day1 extends Day(2021, 1) {
  override def part1(input: Array[String]): String = {
    input.zip(input.tail).count(increasing).toString
  }

  override def part2(input: Array[String]): String = {
    input.zip(input.drop(3)).count(increasing).toString
  }

  private[this] def increasing(e: (String, String)): Boolean = {
    e._2.toInt > e._1.toInt
  }
}

object Day1 {
  def apply() = new Day1()
}

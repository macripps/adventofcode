package aoc2021

import aoc.Day

class Day7 extends Day(2021, 7) {
  override def part1(input: Array[String]): String = {
    val positions = input.head.split(',').map(_.toInt)
    (positions.min to positions.max).map { p =>
      positions.map(pos => cost1(p - pos)).sum
    }.min.toString
  }

  def cost1(a: Int): Int = Math.abs(a)

  override def part2(input: Array[String]): String = {
    val positions = input.head.split(',').map(_.toInt)
    (positions.min to positions.max).map { p =>
      positions.map(pos => cost2(p - pos)).sum
    }.min.toString
  }

  def cost2(a: Int): Int = {
    val b = Math.abs(a)
    (b * (b+1)) / 2
  }
}

object Day7 {
  def apply() = new Day7()
}

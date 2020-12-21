package aoc2020

import aoc.Day
import aoc2020.Day5.{binaryToInt, sumFrom, toBinary}

class Day5 extends Day(2020, 5) {
  override def part1: String = {
    "The highest SeatID is " + input.map(toBinary).map(binaryToInt).max
  }

  override def part2: String = {
    val sorted = input.map(toBinary).map(binaryToInt).sorted
    "The missing SeatID is " + (sumFrom(sorted.head, sorted.last) - sorted.sum)
  }
}

object Day5 {
  def apply() = new Day5()

  def toBinary(line: String): String = {
    line.replace('F', '0')
      .replace('B', '1')
      .replace('R', '1')
      .replace('L', '0')
  }

  def binaryToInt(binaryLine: String): Int = {
    Integer.parseInt(binaryLine, 2)
  }

  def sumFrom(low: Int, high: Int): Int = {
    (low + high) * (high + 1 - low) / 2
  }
}

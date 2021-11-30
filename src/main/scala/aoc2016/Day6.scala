package aoc2016

import aoc.Day

class Day6 extends Day(2016, 6) {
  override def part1(input: Array[String]): String = {
    input.map(_.toCharArray).transpose.map { x: Array[Char] =>
      x.map { c => c -> x.count(_ == c)}.maxBy{case (_, i) => i}._1
    }.mkString
  }

  override def part2(input: Array[String]): String = {
    input.map(_.toCharArray).transpose.map { x: Array[Char] =>
      x.map { c => c -> x.count(_ == c)}.minBy{case (_, i) => i}._1
    }.mkString
  }
}

object Day6 {
  def apply() = new Day6()
}

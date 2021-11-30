package aoc2017

import aoc.Day

class Day2 extends Day(2017, 2) {
  override def part1(input: Array[String]): String = input.map { x =>
    val y = x.split(raw"\s+").map(x => x.toInt)
    y.max - y.min
  }.sum.toString

  override def part2(input: Array[String]): String = input.map { x =>
    x.split(raw"\s+").map(x => x.toInt).sorted.combinations(2).filter(l => l(1)%l(0) == 0).map{z => z(1)/z(0)}.next()
  }.sum.toString
}

object Day2 {
  def apply() = new Day2()
}

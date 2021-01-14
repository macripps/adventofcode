package aoc2018

import aoc.Day

import scala.collection.mutable

class Day1 extends Day(2018, 1) {
  override def part1: String = input.map(_.toInt).sum.toString

  override def part2: String = {
    val i = input.map(_.toInt)
    val seen = mutable.Set[Int]()
    var x = 0
    var p = 0
    while (!seen.contains(x)) {
      seen.addOne(x)
      val n = i(p % i.length)
      x = x + n
      p = p + 1
    }
    x.toString
  }
}

object Day1 {
  def apply() = new Day1()
}

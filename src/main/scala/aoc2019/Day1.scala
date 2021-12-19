package aoc2019

import aoc.Day

class Day1 extends Day(2019, 1) {
  override def part1(input: Array[String]): String = {
    input.map(_.toInt / 3 - 2).sum.toString
  }

  override def part2(input: Array[String]): String = {
    println(fuel(14))
    println(fuel(1969))
    println(fuel(100756))
    input.map{f => fuel(f.toInt)}.sum.toString
  }

  def fuel(f: Int): Int = {
    if (f <= 0) 0 else {
      val m = f / 3 - 2
      if (m < 0) 0 else m + fuel(m)
    }
  }
}

object Day1 {
  def apply() = new Day1()
}

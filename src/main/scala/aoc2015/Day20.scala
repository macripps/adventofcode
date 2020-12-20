package aoc2015

import aoc.Day

class Day20 extends Day {
  override def year: Int = 2015
  override def day: Int = 20

  override def part1(input: Array[String]): String = {
    val target = input.head.toInt

    val houses = Array.ofDim[Int](1_000_000)
    (1 until houses.length).foreach { elf =>
      Range(elf, houses.length-1, elf).foreach { step =>
        houses(step) = houses(step) + (elf * 10)
      }
    }
    houses.indexWhere(k => k >= target).toString
  }

  override def part2(input: Array[String]): String = {
    val target = input.head.toInt

    val houses = Array.ofDim[Int](1_000_000)
    (1 until houses.length).foreach { elf =>
      Range(elf, math.min(50 * elf, houses.length-1), elf).foreach { step =>
        houses(step) = houses(step) + (elf * 11)
      }
    }
    houses.indexWhere(k => k >= target).toString
  }
}

object Day20 {
  def apply() = new Day20()
}

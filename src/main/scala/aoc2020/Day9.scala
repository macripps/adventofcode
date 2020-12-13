package aoc2020

import aoc.Day
import aoc2020.Day9.part1Answer

class Day9 extends Day {
  override def year: Int = 2020

  override def day: Int = 9

  override def part1(input: Array[String]): String = {
    "There was no sum for " + part1Answer(input.map(_.toLong))
  }

  override def part2(input: Array[String]): String = {
    val lines = input.map(_.toLong)
    val search = part1Answer(lines)
    (0 until input.length - 2).flatMap { i =>
      (i + 2 until input.length).map { j =>
        val longs = lines.slice(i, j)
        if (longs.sum == search) {
          val min = longs.min
          val max = longs.max
          "The min and max are " + min + " and " + max + " and sum to " + (min + max)
        } else ""
      }
    }.mkString("")
  }
}

object Day9 {
  def apply() = new Day9()

  private def part1Answer(lines: Array[Long]) = {
    val out = (25 until lines.length).flatMap { i =>
      val sub = lines.slice(i - 25, i)
      val target = lines(i)
      Day1.findPairThatSumTo(sub.sorted, target) match {
        case None => Some(target)
        case Some(_) => None
      }
    }
    val part1Answer = out.head
    part1Answer
  }
}

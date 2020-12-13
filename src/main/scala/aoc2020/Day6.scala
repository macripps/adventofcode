package aoc2020

import aoc.Day

class Day6 extends Day {
  override def year: Int = 2020
  override def day: Int = 6

  override def part1(input: Array[String]): String = {
    val lineGroups = asGroupsSeparatedByBlankLines(input)
    val uniqueAnswers = lineGroups.map { lineGroup =>
      lineGroup.flatMap { l => l }.toSet.size
    }.sum
    "There were " + uniqueAnswers + " unique answers"
  }

  override def part2(input: Array[String]): String = {
    val lineGroups = asGroupsSeparatedByBlankLines(input)

    val commonAnswers = lineGroups.map { groups =>
      groups.foldLeft("abcdefghijklmnopqrstuvwxyz".toSet) { (z, i) =>
        z.intersect(i.toSet)
      }.size
    }.sum
    "There were " + commonAnswers + " common answers"
  }
}

object Day6 {
  def apply() = new Day6()
}

package aoc2020

import aoc.Day

class Day6 extends Day(2020, 6) {
  override def part1: String = {
    val uniqueAnswers = inputGroups.map { lineGroup =>
      lineGroup.flatMap { l => l }.toSet.size
    }.sum
    "There were " + uniqueAnswers + " unique answers"
  }

  override def part2: String = {
    val commonAnswers = inputGroups.map { groups =>
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

package aoc2020

import aoc.NewDay

class Day6 extends NewDay(2020, 6) {
  part(1) {
    execute { in =>
      val uniqueAnswers = inputGroups(in).map { lineGroup =>
        lineGroup.flatMap { l => l }.toSet.size
      }.sum
      "There were " + uniqueAnswers + " unique answers"
    }
  }

  part(2) {
    execute { in =>
      val commonAnswers = inputGroups(in).map { groups =>
        groups.foldLeft("abcdefghijklmnopqrstuvwxyz".toSet) { (z, i) =>
          z.intersect(i.toSet)
        }.size
      }.sum
      "There were " + commonAnswers + " common answers"
    }
  }
}

object Day6Main extends Day6

package aoc2020

import aoc.NewDay
import aoc2020.Day9.part1Answer

class Day9 extends NewDay(2020, 9) {
  part(1) {
    execute { in =>
      "There was no sum for " + part1Answer(in.map(_.toLong))
    }
  }

  part(2) {
    execute { in =>
      val lines = in.map(_.toLong)
      val search = part1Answer(lines)
      (0 until in.length - 2).flatMap { i =>
        (i + 2 until in.length).map { j =>
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
}

object Day9Main extends Day9

object Day9 {
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

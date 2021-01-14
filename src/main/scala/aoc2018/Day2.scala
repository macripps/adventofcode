package aoc2018

import aoc.{Day, Strings}

class Day2 extends Day(2018, 2) {
  override def part1: String = {
    val wcs = input.map { w =>
      w.groupMapReduce(identity)(_ => 1)(_ + _)
    }
    (wcs.count(_.exists(x => x._2 == 2)) * wcs.count(_.exists(x => x._2 == 3))).toString
  }

  override def part2: String = {
    input.combinations(2).filter(xs => Strings.levenshteinDistance(xs(0), xs(1)) == 1).map { xs =>
      xs(0).zipWithIndex.filter(p => xs(1).charAt(p._2) == p._1).map(_._1).mkString
    }.mkString(",")
  }
}

object Day2 {
  def apply() = new Day2()
}

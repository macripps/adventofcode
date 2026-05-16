package aoc2016

import aoc.NewDay

class Day6 extends NewDay(2016, 6) {
  part(1) {
    execute { in =>
      in.map(_.toCharArray).transpose.map { x: Array[Char] =>
        x.map { c => c -> x.count(_ == c)}.maxBy{case (_, i) => i}._1
      }.mkString
    }
  }

  part(2) {
    execute { in =>
      in.map(_.toCharArray).transpose.map { x: Array[Char] =>
        x.map { c => c -> x.count(_ == c)}.minBy{case (_, i) => i}._1
      }.mkString
    }
  }
}

object Day6Main extends Day6

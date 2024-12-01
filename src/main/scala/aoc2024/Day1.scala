package aoc2024

import aoc.NewDay

class Day1 extends NewDay(2024, 1) {

  part(1) {
    test {
      """3   4
        |4   3
        |2   5
        |1   3
        |3   9
        |3   3""".stripMargin -> 11
    }

    execute { input =>
      val pairs = input.map { line =>
        val Array(l, r) = line.split(" +")
        Array(l.toInt,r.toInt)
      }.transpose
      pairs(0).sorted.zip(pairs(1).sorted).map{in => math.abs(in._1 - in._2)}.sum
    }
  }

  part(2) {
    test {
      """3   4
        |4   3
        |2   5
        |1   3
        |3   9
        |3   3""".stripMargin -> 31
    }

    execute { input =>
      val pairs = input.map { line =>
        val Array(l, r) = line.split(" +")
        Array(l.toInt, r.toInt)
      }.transpose
      pairs(0).map { in => in * pairs(1).count(_ == in) }.sum
    }
  }
}

object Day1Main extends Day1

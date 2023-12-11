package aoc2023

import aoc.Maths

class Day6 extends aoc.NewDay(2023, 6) {

  val test1 =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  part(1) {
    test { test1 -> 288L}

    execute { input =>
      val times = input.head.split(" +").tail.map(_.toInt)
      val distances = input(1).split(" +").tail.map(_.toInt)
      times.zip(distances).map { case (t, d) =>
        (0 to t).count {
          n =>
            n * (t - n) > d
        }
      }.product.toLong
    }
  }

  part(2) {
    test { test1 -> 71503L}

    execute { input =>
      val time = input.head.split(" +").tail.mkString.toLong
      val distance = input(1).split(" +").tail.mkString.toLong
      val result = Maths.quadraticSolve(1, -time, distance)
      result.max.toInt - result.min.toInt
    }
  }
}

object Day6Main extends Day6

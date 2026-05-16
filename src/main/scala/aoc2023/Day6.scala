package aoc2023

import aoc.{Maths, NewDay}

class Day6 extends NewDay(2023, 6) {

  part(1) {
    test("""Time:      7  15   30
           |Distance:  9  40  200""".stripMargin -> 288L)

    execute { in =>
      val times = in.head.split(" +").tail.map(_.toInt)
      val distances = in(1).split(" +").tail.map(_.toInt)
      times.zip(distances).map { case (t, d) =>
        (0 to t).count {
          n =>
            n * (t - n) > d
        }
      }.product.toLong
    }
  }

  part(2) {
    test("""Time:      7  15   30
           |Distance:  9  40  200""".stripMargin -> 71503L)

    execute { in =>
      val time = in.head.split(" +").tail.mkString.toLong
      val distance = in(1).split(" +").tail.mkString.toLong
      val result = Maths.quadraticSolve(1, -time, distance)
      result.max.toInt - result.min.toInt
    }
  }
}

object Day6Main extends Day6

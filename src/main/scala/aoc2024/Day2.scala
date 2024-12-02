package aoc2024

import aoc.NewDay

class Day2 extends NewDay(2024, 2) {
  def isSafe(a: Array[Int]): Boolean = {
    (a.forall(_ > 0) && a.min >= 1 && a.max <= 3) || (a.forall(_ < 0) && a.min >= -3 && a.max <= -1)
  }
  part(1) {
    test {
      """7 6 4 2 1
        |1 2 7 8 9
        |9 7 6 2 1
        |1 3 2 4 5
        |8 6 4 4 1
        |1 3 6 7 9""".stripMargin -> 2
    }

    execute { l =>
      l.count { line =>
        val values = line.split("\\s+").map(_.toInt)
        val diffs = values.zip(values.drop(1)).map(p => p._2 - p._1)
        isSafe(diffs)
      }
    }
  }

  part(2) {
    test {
      """7 6 4 2 1
        |1 2 7 8 9
        |9 7 6 2 1
        |1 3 2 4 5
        |8 6 4 4 1
        |1 3 6 7 9""".stripMargin -> 4
    }

    execute { l =>
      l.count { line =>
        val values = line.split("\\s+").map(_.toInt)
        values.indices.exists { i =>
          val elided = values.slice(0, i) ++ values.slice(i+1, values.length)
          val diffs = elided.zip(elided.drop(1)).map(p => p._2 - p._1)
          isSafe(diffs)
        }
      }
    }
  }
}

object Day2Main extends Day2

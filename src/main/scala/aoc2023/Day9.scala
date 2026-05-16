package aoc2023

import aoc.NewDay

class Day9 extends NewDay(2023, 9) {

  val test1 =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  part(1) {
    test(test1 -> 114)

    execute { in =>
      val seqs = in.map { line => line.split(" +").map(_.toLong).toList }.toList
      seqs.map { seq =>
        calculateNextNum(seq)
      }.sum
    }
  }

  part(2) {
    test(test1 -> 2)

    execute { in =>
      val seqs = in.map { line => line.split(" +").map(_.toLong).toList }.toList
      seqs.map { seq =>
        calculatePreviousNum(seq)
      }.sum
    }
  }

  def calculateNextNum(values: List[Long]): Long = {
    if (values.forall(_ == 0L)) {
      0L
    } else {
      val differences = values.indices.dropRight(1).map { i =>
        values(i + 1) - values(i)
      }.toList
      val n = calculateNextNum(differences)
      values.last + n
    }
  }

  def calculatePreviousNum(values: List[Long]): Long = {
    if (values.forall(_ == 0L)) {
      0L
    } else {
      val differences = values.indices.dropRight(1).map { i =>
        values(i + 1) - values(i)
      }.toList
      val n = calculatePreviousNum(differences)
      values.head - n
    }
  }
}

object Day9Main extends Day9

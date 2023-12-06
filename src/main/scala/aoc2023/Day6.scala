package aoc2023

import aoc.Maths

class Day6 extends aoc.Day(2023, 6) {

  withPart1Test(
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin, 288L)

  override def part1(input: Array[String]): Any = {
    val times = input.head.split(" +").tail.map(_.toInt)
    val distances = input(1).split(" +").tail.map(_.toInt)
    times.zip(distances).map { case (t, d) =>
      (0 to t).count {
        n =>
          n * (t - n) > d
      }
    }.product
  }

  withPart2Test(
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin, 71503)

  override def part2(input: Array[String]): Any = {
    val time = input.head.split(" +").tail.mkString.toLong
    val distance = input(1).split(" +").tail.mkString.toLong
    val result = Maths.quadraticSolve(1, -time, distance)
    result.max.toInt - result.min.toInt
  }
}

object Day6 {
  def apply() = new Day6
}

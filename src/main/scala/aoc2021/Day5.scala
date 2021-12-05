package aoc2021

import aoc.{Day, Point}
import aoc2021.Day5.example

class Day5 extends Day(2021, 5) {
  import Day5.parse

  override def part1(input: Array[String]): String = {
    val lines = input.map(parse)

    var ptsCovered = Set[Point]()
    var seenTwice = Set[Point]()
    var numSeenTwice = 0
    lines.filter(p => p._1.x == p._2.x || p._1.y == p._2.y).foreach { line =>
      val onLine = ptsBetween(line._1, line._2)

      onLine.foreach { linePoint =>
        if (ptsCovered.contains(linePoint)) {
          if (!seenTwice.contains(linePoint)) {
            numSeenTwice = numSeenTwice + 1
            seenTwice = seenTwice + linePoint
          }
        } else ptsCovered = ptsCovered + linePoint
      }
    }

    numSeenTwice.toString
  }

  def ptsBetween(point: aoc.Point, point1: aoc.Point): List[Point] = {
    val leftOrRight = if (point.x < point1.x) 1 else if (point.x == point1.x) 0 else -1
    val upOrDown = if (point.y < point1.y) 1 else if (point.y == point1.y) 0 else -1
    var c = point
    var out = List[Point](c)
    while (c != point1) {
      c = Point(c.x + leftOrRight, c.y + upOrDown)
      out = out :+ c
    }
    out
  }

  override def part2(input: Array[String]): String = {
    val lines = input.map(parse)

    var ptsCovered = Set[Point]()
    var seenTwice = Set[Point]()
    var numSeenTwice = 0
    lines.foreach { line =>
      val onLine = ptsBetween(line._1, line._2)

      onLine.foreach { linePoint =>
        if (ptsCovered.contains(linePoint)) {
          if (!seenTwice.contains(linePoint)) {
            numSeenTwice = numSeenTwice + 1
            seenTwice = seenTwice + linePoint
          }
        } else ptsCovered = ptsCovered + linePoint
      }
    }

    numSeenTwice.toString
  }
}

object Day5 {
  def apply() = new Day5

  val example =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin.split("\n")

  def parse(l: String): (Point, Point) = {
    val lr = l.split(" -> ")
    val lrl = lr(0).split(',')
    val lrr = lr(1).split(',')
    (Point(lrl(0).toInt, lrl(1).toInt), Point(lrr(0).toInt, lrr(1).toInt))
  }
}

package aoc2021

import aoc.{NewDay, Point}

class Day5 extends NewDay(2021, 5) {
  import Day5.parse

  part(1) {
    execute { in =>
      val lines = in.map(parse)

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

  part(2) {
    execute { in =>
      val lines = in.map(parse)

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
}

object Day5 {
  def parse(l: String): (Point, Point) = {
    val lr = l.split(" -> ")
    val lrl = lr(0).split(',')
    val lrr = lr(1).split(',')
    (Point(lrl(0).toInt, lrl(1).toInt), Point(lrr(0).toInt, lrr(1).toInt))
  }
}

object Day5Main extends Day5

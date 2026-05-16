package aoc2022

import aoc.NewDay

import scala.collection.mutable

import Day9._

class Day9 extends NewDay(2022, 9) {
  part(1) {
    execute { in =>
      var h = aoc.Point(0, 0)
      var t = aoc.Point(0, 0)
      val visited = mutable.Set[aoc.Point](t)
      in.foreach { line =>
        val (delta, amount) = line match {
          case left(amt) => ((-1, 0), amt.toInt)
          case right(amt) => ((1, 0), amt.toInt)
          case up(amt) => ((0, 1), amt.toInt)
          case down(amt) => ((0, -1), amt.toInt)
        }
        (1 to amount).foreach { _ =>
          h = aoc.Point(h.x + delta._1, h.y + delta._2)
          if (math.abs(h.x - t.x) > 1 || math.abs(h.y - t.y) > 1) {
            val dx = math.signum(h.x - t.x)
            val dy = math.signum(h.y - t.y)
            t = aoc.Point(t.x + dx, t.y + dy)
            visited.add(t)
          }
        }
      }
      visited.size
    }
  }

  part(2) {
    execute { in =>
      val knots = Array(aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0))
      val visited = mutable.Set[aoc.Point](knots(9))
      in.foreach { line =>
        val (delta, amount) = line match {
          case left(amt) => ((-1, 0), amt.toInt)
          case right(amt) => ((1, 0), amt.toInt)
          case up(amt) => ((0, 1), amt.toInt)
          case down(amt) => ((0, -1), amt.toInt)
        }
        (1 to amount).foreach { _ =>
          knots(0) = aoc.Point(knots(0).x + delta._1, knots(0).y + delta._2)
          (1 until knots.length).foreach { knot =>
            val h = knots(knot - 1)
            val t = knots(knot)
            if (math.abs(h.x - t.x) > 1 || math.abs(h.y - t.y) > 1) {
              val dx = math.signum(h.x - t.x)
              val dy = math.signum(h.y - t.y)
              knots(knot) = aoc.Point(t.x + dx, t.y + dy)
            }
          }
          visited.add(knots(9))
        }
      }
      visited.size
    }
  }
}

object Day9Main extends Day9

object Day9 {
  val left = raw"L (\d+)".r
  val right = raw"R (\d+)".r
  val up = raw"U (\d+)".r
  val down = raw"D (\d+)".r
}

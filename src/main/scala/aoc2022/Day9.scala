package aoc2022

import scala.collection.mutable

import Day9._

class Day9 extends aoc.Day(2022, 9) {
  override def part1(input: Array[String]): Any = {
    var h = aoc.Point(0, 0)
    var t = aoc.Point(0, 0)
    val visited = mutable.Set[aoc.Point](t)
    input.foreach { line =>
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

  val test =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val knots = Array(aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0), aoc.Point(0, 0))
    val visited = mutable.Set[aoc.Point](knots(9))
    input.foreach { line =>
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

  val test2 = """R 5
                |U 8
                |L 8
                |D 3
                |R 17
                |D 10
                |L 25
                |U 20""".stripMargin.split("\n")
}

object Day9 {
  def apply() = new Day9

  val left = raw"L (\d+)".r
  val right = raw"R (\d+)".r
  val up = raw"U (\d+)".r
  val down = raw"D (\d+)".r
}

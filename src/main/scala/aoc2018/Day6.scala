package aoc2018

import aoc.{Day, Point}

class Day6 extends Day(2018, 6) {
  val example = Set(
    Point(1, 1),
    Point(1, 6),
    Point(8, 3),
    Point(3, 4),
    Point(5, 5),
    Point(8, 9),
  )

  lazy val points: Set[Point] = input.map(l => {
    val kv = l.split(", ")
    Point(kv(0).toInt, kv(1).toInt)
  }).toSet

  override def part1: String = {
    val xMin = points.minBy(_.x).x
    val xMax = points.maxBy(_.x).x
    val yMin = points.minBy(_.y).y
    val yMax = points.maxBy(_.y).y

    val border = ((yMin to yMax).flatMap { y => Seq(Point(xMin, y), Point(xMax, y)) } ++
      (xMin to xMax).flatMap { x => Seq(Point(x, yMin), Point(x, yMax)) })

    val finitePoints = points.filter { p =>
      isFinite(p, border, points)
    }

    val as = finitePoints.map(p => {
      p -> area(p, xMin, xMax, yMin, yMax, points)
    }).toMap
    as.maxBy(_._2)._2.toString
  }

  def isFinite(p: Point, border: Seq[Point], points: Set[Point]): Boolean = {
    !border.contains(p) && !border.exists { pt =>
      isClosestTo(p, pt, points)
    }
  }

  def area(p: Point, xMin: Int, xMax: Int, yMin: Int, yMax: Int, points: Set[Point]): Int = {
    var area = 0
    (xMin to xMax).foreach { x =>
      (yMin to yMax).foreach { y =>
        if (isClosestTo(p, Point(x, y), points)) {
          area = area + 1
        }
      }
    }
    area
  }

  def isClosestTo(p: Point, pt: Point, points: Set[Point]): Boolean = {
    val d = p.manhattanDistanceTo(pt)
    !points.exists { p2 => {
      p2.manhattanDistanceTo(pt) < d ||
        (p2.manhattanDistanceTo(pt) == d && p2 != p)
    }
    }
  }

  override def part2: String = {
    val xMin = points.minBy(_.x).x
    val xMax = points.maxBy(_.x).x
    val yMin = points.minBy(_.y).y
    val yMax = points.maxBy(_.y).y

    val distance = 10000

    var area = 0
    (xMin to xMax).foreach { x =>
      (yMin to yMax).foreach { y =>
        val d = points.toList.map { p => p.manhattanDistanceTo(Point(x,y)) }.sum
        if (d < distance) {
          area = area + 1
        }
      }
    }
    area.toString
  }
}

object Day6 {
  def apply() = new Day6()
}

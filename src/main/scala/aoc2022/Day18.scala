package aoc2022

import scala.collection.mutable
import scala.util.control.Breaks.breakable

class Day18 extends aoc.Day(2022, 18) {
  override def part1(input: Array[String]): Any = {
    val points = input.map { line =>
      val coords = line.split(",")
      aoc.Point3(coords(0).toInt, coords(1).toInt, coords(2).toInt)
    }
    val pointsSet = points.toSet

    points.map { pt =>
      6 - (pt.neighbours.intersect(pointsSet).size)
    }.sum
  }

  override def part2(input: Array[String]): Any = {
    val points = input.map { line =>
      val coords = line.split(",")
      aoc.Point3(coords(0).toInt, coords(1).toInt, coords(2).toInt)
    }
    val pointsSet = points.toSet
    val allFaces = points.map { pt =>
      6 - (pt.neighbours.intersect(pointsSet).size)
    }.sum

    val minX = points.minBy(pt => pt.x).x
    val minY = points.minBy(pt => pt.y).y
    val minZ = points.minBy(pt => pt.z).z
    val maxX = points.maxBy(pt => pt.x).x
    val maxY = points.maxBy(pt => pt.y).y
    val maxZ = points.maxBy(pt => pt.z).z

    val missing = mutable.Set[aoc.Point3]()
    (minX to maxX).foreach { x =>
      (minY to maxY).foreach { y =>
        (minZ to maxZ).foreach { z =>
          val maybeEmpty = aoc.Point3(x, y, z)
          if (!pointsSet.contains(maybeEmpty)) {
            missing.add(maybeEmpty)
          }
        }
      }
    }

    while (missing.exists(pt => pt.x == minX || pt.x == maxX || pt.y == minY || pt.y == maxY || pt.z == minZ || pt.z == maxZ)) {
      val start = missing.find(pt => pt.x == minX || pt.x == maxX || pt.y == minY || pt.y == maxY || pt.z == minZ || pt.z == maxZ).get
      val toExpand = mutable.ArrayDeque[aoc.Point3](start)
      while (toExpand.nonEmpty) {
        val pt = toExpand.removeHead()
        missing -= pt
        val ns = pt.neighbours.intersect(missing)
        ns.foreach { n =>
          if (!toExpand.contains(n)) {
            toExpand.addOne(n)
          }
        }
      }
    }
    val fillCenter = pointsSet ++ missing
    points.map { pt =>
      6 - (pt.neighbours.intersect(fillCenter).size)
    }.sum
  }

  val test1 =
    """1,1,1
      |2,1,1""".stripMargin.split("\n")

  val test2 =
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5""".stripMargin.split("\n")
}

object Day18 {
  def apply() = new Day18
}
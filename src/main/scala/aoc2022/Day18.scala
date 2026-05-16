package aoc2022

import aoc.NewDay

import scala.collection.mutable

class Day18 extends NewDay(2022, 18) {
  part(1) {
    execute { in =>
      val points = in.map { line =>
        val coords = line.split(",")
        aoc.Point3(coords(0).toInt, coords(1).toInt, coords(2).toInt)
      }
      val pointsSet = points.toSet

      points.map { pt =>
        6 - (pt.neighbours.intersect(pointsSet).size)
      }.sum
    }
  }

  part(2) {
    execute { in =>
      val points = in.map { line =>
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
  }
}

object Day18Main extends Day18

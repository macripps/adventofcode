package aoc2025

import aoc.{NewDay, Point}

import scala.collection.mutable

class Day9 extends NewDay(2025, 9) {

  part(1) {
    test("""7,1
           |11,1
           |11,7
           |9,7
           |9,5
           |2,5
           |2,3
           |7,3""".stripMargin -> 50L)
    execute { ls =>
      val pts = ls.map { l =>
        val Array(x,y) = l.split(',')
        Point(x.toInt, y.toInt)
      }
      (0 until pts.length - 2).flatMap { p1 =>
        (p1+1 until pts.length).map { p2 =>
          val dx = math.abs(pts(p1).x - pts(p2).x) + 1
          val dy = math.abs(pts(p1).y - pts(p2).y) + 1
          dx.toLong * dy.toLong
        }
      }.max
    }
  }

  part(2) {
    test("""7,1
           |11,1
           |11,7
           |9,7
           |9,5
           |2,5
           |2,3
           |7,3""".stripMargin -> 24L)
    execute { ls =>
      val pts = ls.map { l =>
        val Array(x,y) = l.split(',').map(_.toInt)
        Point(x, y)
      }
      val rectangles = mutable.LinkedHashSet[(Int,Int,Int,Int)]() // left,right,bottom,top
      pts.indices.foreach { pt1Idx =>
        val pt2Idx = (pt1Idx + 1) % pts.length
        val pt1 = pts(pt1Idx)
        val pt2 = pts(pt2Idx)
        rectangles.add((math.min(pt1.x, pt2.x), math.max(pt1.x, pt2.x), math.min(pt1.y, pt2.y), math.max(pt1.y, pt2.y)))
      }
      pts.flatMap { pt1 =>
        pts.map { pt2 =>
          // Rectangle from pt1 => pt2
          val minX = math.min(pt1.x, pt2.x)
          val maxX = math.max(pt1.x, pt2.x)
          val minY = math.min(pt1.y, pt2.y)
          val maxY = math.max(pt1.y, pt2.y)
          val size = if (rectangles.exists { case (rL, rR, rB, rT) =>
            (minX < rR && maxX > rL && minY < rT && maxY > rB)
          }) { 0L } else {
            (math.abs((pt2.x-pt1.x)).toLong + 1)*(math.abs(pt2.y-pt1.y).toLong + 1)
          }
          size
        }
      }.max
    }
  }
}

object Day9Main extends Day9

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
      var minX = Int.MaxValue
      var maxX = Int.MinValue
      var minY = Int.MaxValue
      var maxY = Int.MinValue
      val pts = ls.map { l =>
        val Array(x,y) = l.split(',').map(_.toInt)
        if (x < minX) { minX = x }
        if (x > maxX) { maxX = x }
        if (y < minY) { minY = y }
        if (y > maxY) { maxY = y }
        Point(x, y)
      }
      val redGreenSet = mutable.LinkedHashSet[Point]()
      pts.indices.foreach { pt1Idx =>
        val pt2Idx = (pt1Idx + 1) % pts.length
        val pt1 = pts(pt1Idx)
        val pt2 = pts(pt2Idx)
        if (pt1.x == pt2.x) {
          pt1.y.to(pt2.y, if (pt2.y > pt1.y) 1 else -1).foreach { y =>
            redGreenSet.add(Point(pt1.x, y))
          }
        } else {
          pt1.x.to(pt2.x, if (pt2.x > pt1.x) 1 else -1).foreach { x =>
            redGreenSet.add(Point(x, pt1.y))
          }
        }
      }
      var s = Point(minX, minY)
      while (!redGreenSet.contains(s)) {
        s = Point(s.x + 1, s.y + 1)
      }
      s = Point(s.x + 1, s.y + 1)
      val open = mutable.Queue[Point](s)
      while (open.nonEmpty) {
        val next = open.dequeue()
        redGreenSet.addOne(next)
        open.enqueueAll(next.neighbours.filter { p => !redGreenSet.contains(p) })
      }
      (0 until pts.length - 2).flatMap { p1Idx =>
        (p1Idx+1 until pts.length).filter { p2Idx =>
          val p1 = pts(p1Idx)
          val p2 = pts(p2Idx)
          val allInSet = p1.x.to(p2.x, if (p2.x > p1.x) 1 else -1).forall { x =>
            p1.y.to(p2.y, if (p2.y > p1.y) 1 else -1).forall { y =>
              redGreenSet.contains(Point(x, y))
            }
          }
          allInSet
        }.map { p2Idx =>
          val dx = math.abs(pts(p1Idx).x - pts(p2Idx).x) + 1
          val dy = math.abs(pts(p1Idx).y - pts(p2Idx).y) + 1
          dx.toLong * dy.toLong
        }
      }.max
    }
  }
}

object Day9Main extends Day9

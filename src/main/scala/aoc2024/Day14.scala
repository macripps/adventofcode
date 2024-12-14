package aoc2024

import aoc.{NewDay, Point}

class Day14 extends NewDay(2024, 14) {

  part(1) {
    test(
      """11,7
        |p=0,4 v=3,-3
        |p=6,3 v=-1,-3
        |p=10,3 v=-1,2
        |p=2,0 v=2,-1
        |p=0,0 v=1,3
        |p=3,0 v=-2,-2
        |p=7,6 v=-1,-3
        |p=3,0 v=-1,-2
        |p=9,3 v=2,3
        |p=7,3 v=-1,2
        |p=2,4 v=2,-3
        |p=9,5 v=-3,-3""".stripMargin -> 12)
    execute { ls =>
      val Array(mX, mY) = ls.head.split(",").map(_.toInt)
      val robots = ls.tail.map { l =>
        val Array(p, v) = l.split(" ")
        val Array(px, py) = p.drop(2).split(",").map(_.toInt)
        val Array(dx, dy) = v.drop(2).split(",").map(_.toInt)
        Robot(Point(px, py), dx, dy)
      }
      val finalPositions = robots.map { r =>
        var nX = (r.pos.x + 100 * r.dX) % mX
        if (nX < 0) {
          nX = nX + mX
        }
        var nY = (r.pos.y + 100 * r.dY) % mY
        if (nY < 0) {
          nY = nY + mY
        }
        Robot(Point(nX, nY), r.dX, r.dY)
      }
      val topLeft = finalPositions.filter { r => r.pos.x < mX / 2 && r.pos.y < mY / 2 }.size
      val topRight = finalPositions.filter { r => r.pos.x > mX / 2 && r.pos.y < mY / 2 }.size
      val bottomLeft = finalPositions.filter { r => r.pos.x < mX / 2 && r.pos.y > mY / 2 }.size
      val bottomRight = finalPositions.filter { r => r.pos.x > mX / 2 && r.pos.y > mY / 2 }.size
      topLeft * topRight * bottomLeft * bottomRight
    }
  }

  part(2) {
    execute { ls =>
      val Array(mX, mY) = ls.head.split(",").map(_.toInt)
      val robots = ls.tail.map { l =>
        val Array(p, v) = l.split(" ")
        val Array(px, py) = p.drop(2).split(",").map(_.toInt)
        val Array(dx, dy) = v.drop(2).split(",").map(_.toInt)
        Robot(Point(px, py), dx, dy)
      }
      val seconds = 7492
      val newRobots = robots.map { r =>
        var nX = (r.pos.x + seconds * r.dX) % mX
        if (nX < 0) {
          nX = nX + mX
        }
        var nY = (r.pos.y + seconds * r.dY) % mY
        if (nY < 0) {
          nY = nY + mY
        }
        Robot(Point(nX, nY), r.dX, r.dY)
      }
      val points = newRobots.map(_.pos).toSet
      (1 to mX).foreach { x =>
        (1 to mY).foreach { y =>
          if (points.contains(Point(x, y))) {
            print('#')
          } else {
            print('.')
          }
        }
        println
      }
      seconds
    }
  }

  case class Robot(pos: Point, dX: Int, dY: Int)
}

object Day14Main extends Day14

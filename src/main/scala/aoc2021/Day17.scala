package aoc2021

import aoc.{NewDay, Point}
import aoc2021.Day17.{parsed, pointInArea}

class Day17 extends NewDay(2021, 17) {
  part(1) {
    execute { in =>
      val targetArea = parsed(in.head)
      var maxY = Int.MinValue
      (1 to targetArea._2.x).foreach { x =>
        (targetArea._1.y to 1000).foreach { y =>
          var position = Point(0,0)
          var xVel = x
          var yVel = y
          var hitArea = false
          var thisMax = Int.MinValue
          while (position.x <= targetArea._2.x && position.y >= targetArea._1.y) {
            position = Point(position.x + xVel, position.y + yVel)
            if (pointInArea(position, targetArea)) {
              hitArea = true
            }
            if (position.y > thisMax) {
              thisMax = position.y
            }
            if (xVel != 0) {
              xVel = xVel - 1
            }
            yVel = yVel - 1
          }
          if (hitArea) {
            if (thisMax > maxY) {
              maxY = thisMax
            }
          }
        }
      }
      maxY.toString
    }
  }

  part(2) {
    execute { in =>
      val targetArea = parsed(in.head)
      var hits = 0
      (1 to targetArea._2.x).foreach { x =>
        (targetArea._1.y to 1000).foreach { y =>
          var position = Point(0,0)
          var xVel = x
          var yVel = y
          var hitArea = false
          while (position.x <= targetArea._2.x && position.y >= targetArea._1.y) {
            position = Point(position.x + xVel, position.y + yVel)
            if (pointInArea(position, targetArea)) {
              hitArea = true
            }
            if (xVel != 0) {
              xVel = xVel - 1
            }
            yVel = yVel - 1
          }
          if (hitArea) {
            hits = hits + 1
          }
        }
      }


      hits.toString
    }
  }
}

object Day17 {
  def parsed(line: String): (Point, Point) = {
    val l1 = line.substring("target area: x=".length)
    val l2 = l1.split(", y=")
    val x = l2(0).split("\\.\\.")
    val y = l2(1).split("\\.\\.")
    (Point(x(0).toInt, y(0).toInt), Point(x(1).toInt, y(1).toInt))
  }

  def pointInArea(pt: Point, area: (Point, Point)): Boolean = {
    pt.x >= area._1.x && pt.x <= area._2.x && pt.y >= area._1.y && pt.y <= area._2.y
  }
}

object Day17Main extends Day17

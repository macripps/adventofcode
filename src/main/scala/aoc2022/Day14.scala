package aoc2022

import aoc.NewDay

import scala.collection.mutable

class Day14 extends NewDay(2022, 14) {
  part(1) {
    execute { in =>
      val source = aoc.Point(500, 0)
      val grid = mutable.Map[aoc.Point, Char]()
      var lowestY = Int.MinValue
      in.foreach { line =>
        val corners = line.split(" -> ")
        val starts = corners.head.split(",")
        var start = aoc.Point(starts(0).toInt, starts(1).toInt)
        corners.tail.foreach { corner =>
          val ends = corner.split(",")
          var end = aoc.Point(ends(0).toInt, ends(1).toInt)
          (math.min(start.x, end.x) to math.max(start.x, end.x)).foreach { x =>
            (math.min(start.y, end.y) to math.max(start.y, end.y)).foreach { y =>
              val point = aoc.Point(x, y)
              if (y > lowestY) {
                lowestY = y
              }
              grid(point) = '#'
            }
          }

          start = end
        }
      }
      var at = source
      var produced = 1
      while (at.y <= lowestY) {
        val down = aoc.Point(at.x, at.y + 1)
        val downLeft = aoc.Point(at.x-1, at.y + 1)
        val downRight = aoc.Point(at.x+1, at.y + 1)
        if (!grid.contains(down)) {
          grid.remove(at)
          grid(down) = 'o'
          at = down
        } else if (!grid.contains(downLeft)) {
          grid.remove(at)
          grid(downLeft) = 'o'
          at = downLeft
        } else if (!grid.contains(downRight)) {
          grid.remove(at)
          grid(downRight) = 'o'
          at = downRight
        } else {
          grid(at) = 'o'
          at = source
          produced = produced + 1
        }
      }
      produced - 1
    }
  }

  part(2) {
    execute { in =>
      val source = aoc.Point(500, 0)
      var lowestY = Int.MinValue
      val grid = mutable.Map[aoc.Point, Char]()
      in.foreach { line =>
        val corners = line.split(" -> ")
        val starts = corners.head.split(",")
        var start = aoc.Point(starts(0).toInt, starts(1).toInt)
        corners.tail.foreach { corner =>
          val ends = corner.split(",")
          var end = aoc.Point(ends(0).toInt, ends(1).toInt)
          (math.min(start.x, end.x) to math.max(start.x, end.x)).foreach { x =>
            (math.min(start.y, end.y) to math.max(start.y, end.y)).foreach { y =>
              val point = aoc.Point(x, y)
              if (y > lowestY) {
                lowestY = y
              }
              grid(point) = '#'
            }
          }

          start = end
        }
      }
      var at = source
      var produced = 1
      while (!grid.contains(source)) {
        val down = aoc.Point(at.x, at.y + 1)
        val downLeft = aoc.Point(at.x-1, at.y + 1)
        val downRight = aoc.Point(at.x+1, at.y + 1)
        if (at.y == lowestY + 1) {
          grid(at) = 'o'
          at = source
          produced = produced + 1
        } else if (!grid.contains(down)) {
          grid.remove(at)
          grid(down) = 'o'
          at = down
        } else if (!grid.contains(downLeft)) {
          grid.remove(at)
          grid(downLeft) = 'o'
          at = downLeft
        } else if (!grid.contains(downRight)) {
          grid.remove(at)
          grid(downRight) = 'o'
          at = downRight
        } else {
          grid(at) = 'o'
          at = source
          produced = produced + 1
        }
      }
      produced - 1
    }
  }
}

object Day14Main extends Day14

package aoc2024

import aoc.{NewDay, Point, Search}

import scala.util.control.Breaks.{break, breakable}

class Day18 extends NewDay(2024, 18) {
  part(1) {
    test(
      """6x6
        |5,4
        |4,2
        |4,5
        |3,0
        |2,1
        |6,3
        |2,4
        |1,5
        |0,6
        |3,3
        |2,6
        |5,1""".stripMargin -> 22)
    execute { ls =>
      val Array(xDim, yDim) = ls(0).split("x").map(_.toInt)
      val grid = Array.ofDim[Array[Boolean]](yDim + 1)
      grid.indices.foreach { y =>
        grid(y) = Array.ofDim[Boolean](xDim + 1)
      }
      val bytes = ls.slice(1, 1025).map { s =>
        val Array(x, y) = s.split(',').map(_.toInt)
        Point(x, y)
      }
      bytes.foreach { p =>
        grid(p.y)(p.x) = true
      }
      val start = Point(0, 0)
      val goal = Point(xDim, yDim)
      val route = Search.AStarWorking[Point](start, g => g == goal, p => p.neighbours.filter { a =>
        a.x >= 0 && a.x <= xDim && a.y >= 0 && a.y <= yDim && !grid(a.y)(a.x)
      }, _ => 1, p => p.manhattanDistanceTo(goal))
      route.length - 1
    }
  }

  part(2) {
    test(
      """6x6
        |5,4
        |4,2
        |4,5
        |3,0
        |2,1
        |6,3
        |2,4
        |1,5
        |0,6
        |3,3
        |2,6
        |5,1
        |1,2
        |5,5
        |2,5
        |6,5
        |1,4
        |0,4
        |6,4
        |1,1
        |6,1
        |1,0
        |0,5
        |1,6
        |2,0""".stripMargin -> "6,1")
    execute { ls =>
      val Array(xDim, yDim) = ls(0).split("x").map(_.toInt)
      val grid = Array.ofDim[Array[Boolean]](yDim + 1)
      grid.indices.foreach { y =>
        grid(y) = Array.ofDim[Boolean](xDim + 1)
      }
      val bytes = ls.tail.map { s =>
        val Array(x, y) = s.split(',').map(_.toInt)
        Point(x, y)
      }
      val start = Point(0, 0)
      val goal = Point(xDim, yDim)
      var currentRoute = Search.AStarWorking[Point](start, g => g == goal, p => p.neighbours.filter { a =>
        a.x >= 0 && a.x <= xDim && a.y >= 0 && a.y <= yDim && !grid(a.y)(a.x)
      }, _ => 1, p => p.manhattanDistanceTo(goal))
      bytes.find { p =>
        grid(p.y)(p.x) = true
        if (currentRoute.contains(p)) {
          currentRoute = Search.AStarWorking[Point](start, g => g == goal, p => p.neighbours.filter { a =>
            a.x >= 0 && a.x <= xDim && a.y >= 0 && a.y <= yDim && !grid(a.y)(a.x)
          }, _ => 1, p => p.manhattanDistanceTo(goal))
          if (debug() && currentRoute.nonEmpty) {
            println("-----")
            grid.indices.foreach { r =>
              grid(r).indices.foreach { c =>
                print(if (currentRoute.contains(Point(c, r))) 'O' else if (grid(r)(c)) '#' else '.')
              }
              println
            }
          }
          currentRoute.isEmpty
        } else false
      }.map { p =>
        s"${p.x},${p.y}"
      }.get
    }
  }
}

object Day18Main extends Day18

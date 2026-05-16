package aoc2021

import aoc.{NewDay, Point}

import scala.collection.mutable

class Day25 extends NewDay(2021, 25) {
  part(1) {
    execute { in =>
      val grid = in.map(_.toCharArray)
      var step = 0
      var moved = true
      while (moved) {
        if (debug()) {
          println("==== " + step + " ====")
          println(grid.map(_.mkString).mkString("\n"))
        }
        moved = false
        step = step + 1
        val toMoveEast = mutable.Set[Point]()
        grid.indices.foreach { r =>
          grid(r).indices.foreach { c =>
            if (grid(r)(c) == '>') {
              if (grid(r)((c + 1) % grid(r).length) == '.') {
                toMoveEast.addOne(Point(c, r))
              }
            }
          }
        }
        toMoveEast.foreach { p =>
          grid(p.y)(p.x) = '.'
          grid(p.y)((p.x + 1) % grid(p.y).length) = '>'
        }
        if (toMoveEast.nonEmpty) {
          moved = true
        }
        val toMoveSouth = mutable.Set[Point]()
        grid.indices.foreach { r =>
          grid(r).indices.foreach { c =>
            if (grid(r)(c) == 'v') {
              if (grid((r + 1) % grid.length)(c) == '.') {
                toMoveSouth.addOne(Point(c, r))
              }
            }
          }
        }
        toMoveSouth.foreach { p =>
          grid(p.y)(p.x) = '.'
          grid((p.y + 1) % grid.length)(p.x) = 'v'
        }
        if (toMoveSouth.nonEmpty) {
          moved = true
        }
      }
      step.toString
    }
  }

  part(2) {
    execute { _ =>
      "Merry Christmas"
    }
  }
}

object Day25Main extends Day25

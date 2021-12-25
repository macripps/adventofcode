package aoc2021

import aoc.{Day, Point}

import scala.collection.mutable

class Day25 extends Day(2021, 25) {
  override def part1(input: Array[String]): String = {
    val grid = input.map(_.toCharArray)
    var step = 0
    var moved = true
    while (moved) {
      if (debug) {
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

  override def part2(input: Array[String]): String = {
    "Merry Christmas"
  }
}

object Day25 {
  def apply() = new Day25

  val example = """v...>>.vv>
                  |.vv>>.vv..
                  |>>.>v>...v
                  |>>v>>.>.v.
                  |v>v.vv.v..
                  |>.>>..v...
                  |.vv..>.>v.
                  |v.v..>>v.v
                  |....v..v.>""".stripMargin.split("\n")
}

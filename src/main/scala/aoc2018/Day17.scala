package aoc2018

import aoc.{Day, Point}

import scala.collection.mutable

class Day17 extends Day(2018, 17) {
  import Day17._

  val example = """x=495, y=2..7
                  |y=7, x=495..501
                  |x=501, y=3..7
                  |x=498, y=2..4
                  |x=506, y=1..2
                  |x=498, y=10..13
                  |x=504, y=10..13
                  |y=13, x=498..504""".stripMargin.split("\n")

  override def part1: String = {
    val input = example
    var minX = Int.MaxValue
    var maxX = Int.MinValue
    var minY = 0
    var maxY = Int.MinValue
    val grid = mutable.Map[Point, Char]((Point(500, 0), '+'))
    input.foreach {
      case vertical(xS: String, yMinS: String, yMaxS: String) =>
        val x = xS.toInt
        val nY = yMinS.toInt
        val xY = yMaxS.toInt
        if (x > maxX) {
          maxX = x
        } else if (x < minX) {
          minX = x
        }
        if (xY > maxY) {
          maxY = xY
        }
        (nY to xY).foreach { y =>
          grid(Point(x, y)) = '#'
        }
      case horizontal(yS: String, xMinS: String, xMaxS: String) =>
        val y = yS.toInt
        val nX = xMinS.toInt
        val xX = xMaxS.toInt
        if (y > maxY) {
          maxY = y
        }
        if (nX < minX) {
          minX = nX
        }
        if (xX > maxX) {
          maxX = xX
        }
        (nX to xX).foreach { x =>
          grid(Point(x, y)) = '#'
        }
    }
    println((minX, minY), (maxX, maxY))
    var changes = true
    while (changes) {
      changes = false
      // Check for source
      val sources = grid.filter(x => {
        val s = Point(x._1.x, x._1.y + 1)
        grid.getOrElse(s, '.') == '.' && x._2 == '+'
      })
      sources.foreach { pt =>
        changes = true
        val s = Point(pt._1.x, pt._1.y + 1)
        grid(s) = '|'
      }
      // Check for falling water
      val falling = grid.filter(x => {
        val s = Point(x._1.x, x._1.y + 1)
        grid.getOrElse(s, '.') == '.' && x._2 == '|'
      })
      falling.foreach { pt =>
        changes = true
        val s = Point(pt._1.x, pt._1.y + 1)
        grid(s) = '|'
      }
      // Check for water that has hit the clay
      val stopped = grid.filter(x => {
        val s = Point(x._1.x, x._1.y + 1)
        grid.getOrElse(s, '.') == '#' && x._2 == '|'
      })
      stopped.foreach { pt =>
        changes = true
        grid(pt._1) = '~'
      }
      // Check for water that can spread horizontally
      val spreading = grid.filter(x => {
        val l = Point(x._1.x-1, x._1.y)
        val r = Point(x._1.x+1, x._1.y)
        (grid.getOrElse(l, '.') == '.' || grid.getOrElse(r, '.') == '.') && x._2 == '~'
      })
      spreading.foreach { pt =>
        changes = true
        val l = Point(pt._1.x-1, pt._1.y)
        val r = Point(pt._1.x+1, pt._1.y)
        if (grid.getOrElse(l, '.') == '.') {
          grid(l) = '~'
        }
        if (grid.getOrElse(r, '.') == '.') {
          grid(r) = '~'
        }
      }
    }
    if (debug) {
      renderGrid(grid, minX, maxX, minY, maxY)
      println()
    }
    ""
  }

  def renderGrid(grid: mutable.Map[aoc.Point, Char], xMin: Int, xMax: Int, yMin: Int, yMax: Int): Unit = {
    (yMin to yMax).foreach { y =>
      (xMin to xMax).foreach { x =>
        val c = grid.getOrElse(Point(x, y), '.')
        print(c)
      }
      println()
    }
  }

  override def part2: String = {
    ""
  }
}

object Day17 {
  def apply() = new Day17()

  val vertical = raw"x=(\d+), y=(\d+)..(\d+)".r
  val horizontal = raw"y=(\d+), x=(\d+)..(\d+)".r
}

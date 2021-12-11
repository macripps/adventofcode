package aoc2021

import aoc.{Day, Point}

class Day11 extends Day(2021, 11) {
  val colors = Array(
    "\u001B[38;2;0;0;0m", // 0 -> Black
    "\u001B[38;2;43;35;41m", // 1 ->
    "\u001B[38;2;66;57;85m", // 2 ->
    "\u001B[38;2;62;66;106m", // 3 ->
    "\u001B[38;2;64;96;128m", // 4 ->
    "\u001B[38;2;62;142;149m", // 5 ->
    "\u001B[38;2;57;170;132m", // 6 ->
    "\u001B[38;2;48;191;84m", // 7 ->
    "\u001B[38;2;65;212;35m", // 8 ->
    "\u001B[38;2;255;255;0m", // 9 -> Yellow
    "\u001B[38;2;15m", // 10 -> White
  )
  val reset = "\u001B[0m"
  val cls = "\u001B[2J"
  val example =
    """11111
      |19991
      |19191
      |19991
      |11111""".stripMargin.split("\n")

  val example2 =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin.split("\n")

  override def part1(input: Array[String]): String = {
    val grid = input.map(_.toCharArray.map(_ - '0'))
    var flashes = 0
    (1 to 100).foreach { step =>
      println(step)
      println(grid.map{c => c.map{i => colors(i) + i}.mkString("")}.mkString("\n") + reset)
      Thread.sleep(500)
      println(cls)
      grid.indices.foreach { y =>
        grid(y).indices.foreach { x =>
          grid(y)(x) = grid(y)(x) + 1
        }
      }
      var flashed = true
      while (flashed) {
        flashed = false
        grid.indices.foreach { y =>
          grid(y).indices.foreach { x =>
            if (grid(y)(x) > 9) {
              flashed = true
              flashes = flashes + 1
              grid(y)(x) = 0
              allSafeNeighbours(y, x, grid.length, grid(y).length).foreach { pt =>
                if (grid(pt.y)(pt.x) != 0) {
                  grid(pt.y)(pt.x) = grid(pt.y)(pt.x) + 1
                }
              }
            }
          }
        }
      }
    }
    flashes.toString
  }

  def allSafeNeighbours(y: Int, x: Int, yMax: Int, xMax: Int): Seq[Point] = {
    var o = Seq[Point]()
    if (y > 0) {
      o = o :+ Point(x, y - 1)
      if (x > 0) {
        o = o :+ Point(x - 1, y - 1)
      }
      if (x < xMax - 1) {
        o = o :+ Point(x + 1, y - 1)
      }
    }
    if (x > 0) {
      o = o :+ Point(x - 1, y)
    }
    if (x < xMax - 1) {
      o = o :+ Point(x + 1, y)
    }
    if (y < yMax - 1) {
      o = o :+ Point(x, y + 1)
      if (x > 0) {
        o = o :+ Point(x - 1, y + 1)
      }
      if (x < xMax - 1) {
        o = o :+ Point(x + 1, y + 1)
      }
    }
    o
  }

  override def part2(input: Array[String]): String = {
    val grid = input.map(_.toCharArray.map(_ - '0'))
    var step = 0
    var flashes = 0
    while (flashes != 100) {
      step = step + 1
      flashes = 0
      grid.indices.foreach { y =>
        grid(y).indices.foreach { x =>
          grid(y)(x) = grid(y)(x) + 1
        }
      }
      var flashed = true
      while (flashed) {
        flashed = false
        grid.indices.foreach { y =>
          grid(y).indices.foreach { x =>
            if (grid(y)(x) > 9) {
              flashed = true
              flashes = flashes + 1
              grid(y)(x) = 0
              allSafeNeighbours(y, x, grid.length, grid(y).length).foreach { pt =>
                if (grid(pt.y)(pt.x) != 0) {
                  grid(pt.y)(pt.x) = grid(pt.y)(pt.x) + 1
                }
              }
            }
          }
        }
      }
    }
    step.toString
  }
}

object Day11 {
  def apply() = new Day11
}

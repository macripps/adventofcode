package aoc2015

import aoc.Day
import aoc2015.Day18._

class Day18 extends Day(2015, 18) {
  override def part1(input: Array[String]): String = {
    var grid = input.map(_.toCharArray)

    (1 to 100).foreach { _ =>
      val next = Array.ofDim[Char](grid.length, grid(0).length)

      grid.indices.foreach { y =>
        grid(y).indices.foreach { x =>
          val n = neighbours(grid, y, x)
          if ((grid(y)(x) == '#' && (n == 2 || n == 3)) || (grid(y)(x) == '.' && n == 3)) {
            next(y)(x) = '#'
          } else {
            next(y)(x) = '.'
          }
        }
      }

      grid = next
    }

    grid.map { r => r.count(c => c == '#') }.sum.toString
  }

  override def part2(input: Array[String]): String = {
    var grid = input.map(_.toCharArray)

    (1 to 100).foreach { _ =>
      val next = Array.ofDim[Char](grid.length, grid(0).length)

      grid.indices.foreach { y =>
        grid(y).indices.foreach { x =>
          val n = neighbours(grid, y, x)
          if ((grid(y)(x) == '#' && (n == 2 || n == 3)) || (grid(y)(x) == '.' && n == 3)) {
            next(y)(x) = '#'
          } else {
            next(y)(x) = '.'
          }
        }
      }

      next(0)(0) = '#'
      next(0)(next(0).length - 1) = '#'
      next(next.length - 1)(0) = '#'
      next(next.length - 1)(next(next.length - 1).length - 1) = '#'

      grid = next
    }

    println(grid.map(r => r.mkString("")).mkString("\n"))

    grid.map { r => r.count(c => c == '#') }.sum.toString
  }
}

object Day18 {
  def apply() = new Day18()

  def neighbours(grid: Array[Array[Char]], y: Int, x: Int): Int = {
    var neighbours = 0
    (math.max(0, y - 1) to math.min(grid.length - 1, y + 1)).foreach { dy =>
      (math.max(0, x - 1) to math.min(grid(y).length - 1, x + 1)).foreach { dx =>
        if (!(dy == y && dx == x)) {
          if (grid(dy)(dx) == '#') {
            neighbours = neighbours + 1
          }
        }
      }
    }
    neighbours
  }
}

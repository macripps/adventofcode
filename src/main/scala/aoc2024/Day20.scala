package aoc2024

import aoc.{Direction, NewDay, Point, Search}

import scala.collection.mutable

class Day20 extends NewDay(2024, 20) {
  part(1) {
    execute { ls =>
      val grid = ls.map(_.toCharArray)
      val start = findInGrid(grid, 'S')
      val end = findInGrid(grid, 'E')
      val path = Search.AStarWorking[Point](start, g => g == end, p => p.neighbours.filter { c => grid(c.y)(c.x) != '#' }, _ => 1, c => c.manhattanDistanceTo(end))

      var timeSaves = 0
      path.indices.foreach { i =>
        val pt = path(i)
        Array(Direction.North, Direction.East, Direction.South, Direction.West).foreach { d =>
          val possibleJump = pt.go(d).go(d)
          val possibleSkip = path.indexOf(possibleJump) - 2
          if (possibleSkip >= i + 100) {
            timeSaves = timeSaves + 1
          }
        }
      }
      timeSaves
    }
  }
  part(2) {
    test(
      """###############
        |#...#...#.....#
        |#.#.#.#.#.###.#
        |#S#...#.#.#...#
        |#######.#.#.###
        |#######.#.#...#
        |#######.#.###.#
        |###..E#...#...#
        |###.#######.###
        |#...###...#...#
        |#.#####.#.###.#
        |#.#...#.#.#...#
        |#.#.#.#.#.#.###
        |#...#...#...###
        |###############""".stripMargin -> 0)
    execute { ls =>
      val grid = ls.map(_.toCharArray)
      val start = findInGrid(grid, 'S')
      val end = findInGrid(grid, 'E')
      val path = Search.AStarWorking[Point](start, g => g == end, p => p.neighbours.filter { c => grid(c.y)(c.x) != '#' }, _ => 1, c => c.manhattanDistanceTo(end))

      var timeSaves = 0
      val max = 100
      path.indices.dropRight(max).foreach { i =>
        path.indices.drop(i + max).foreach { j =>
          if (path(i).manhattanDistanceTo(path(j)) <= 20) {
            if ((j - i) - path(i).manhattanDistanceTo(path(j)) >= max) {
              timeSaves = timeSaves + 1
            }
          }
        }
      }
      timeSaves
    }
  }

  private[this] def findInGrid(grid: Array[Array[Char]], ch: Char): Point = {
    grid.indices.find { c =>
      grid(c).indices.exists { r => grid(c)(r) == ch }
    }.flatMap { c =>
      grid(c).indices.find { r => grid(c)(r) == ch }.map { r =>
        Point(r, c)
      }
    }.get
  }
}

object Day20Main extends Day20

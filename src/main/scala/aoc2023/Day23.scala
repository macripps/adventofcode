package aoc2023

import aoc.{NewDay, Point}

import scala.annotation.tailrec
import scala.collection.mutable

class Day23 extends NewDay(2023, 23) {

  part(1) {
    test {
      """#.#####################
        |#.......#########...###
        |#######.#########.#.###
        |###.....#.>.>.###.#.###
        |###v#####.#v#.###.#.###
        |###.>...#.#.#.....#...#
        |###v###.#.#.#########.#
        |###...#.#.#.......#...#
        |#####.#.#.#######.#.###
        |#.....#.#.#.......#...#
        |#.#####.#.#.#########v#
        |#.#...#...#...###...>.#
        |#.#.#v#######v###.###v#
        |#...#.>.#...>.>.#.###.#
        |#####v#.#.###v#.#.###.#
        |#.....#...#...#.#.#...#
        |#.#########.###.#.#.###
        |#...###...#...#...#.###
        |###.###.#.###v#####v###
        |#...#...#.#.>.>.#.>.###
        |#.###.###.#.###.#.#v###
        |#.....###...###...#...#
        |#####################.#""".stripMargin -> 94
    }

    execute { input =>
      go(input.map(_.toCharArray), Point(1, 0), Point(input(0).length - 2, input.length - 1))
    }
  }


  part(2) {
    test {
      """#.#####################
        |#.......#########...###
        |#######.#########.#.###
        |###.....#.>.>.###.#.###
        |###v#####.#v#.###.#.###
        |###.>...#.#.#.....#...#
        |###v###.#.#.#########.#
        |###...#.#.#.......#...#
        |#####.#.#.#######.#.###
        |#.....#.#.#.......#...#
        |#.#####.#.#.#########v#
        |#.#...#...#...###...>.#
        |#.#.#v#######v###.###v#
        |#...#.>.#...>.>.#.###.#
        |#####v#.#.###v#.#.###.#
        |#.....#...#...#.#.#...#
        |#.#########.###.#.#.###
        |#...###...#...#...#.###
        |###.###.#.###v#####v###
        |#...#...#.#.>.>.#.>.###
        |#.###.###.#.###.#.#v###
        |#.....###...###...#...#
        |#####################.#""".stripMargin -> 154
    }

    execute { input =>
      val grid = input.map(_.toCharArray)
      val start = Point(1, 0)
      val end = Point(input(0).length - 2, input.length - 1)
      go2(grid, start, end)
    }
  }

  private[this] def go(grid: Array[Array[Char]], start: Point, end: Point): Int = {
    var longestPath = Int.MinValue
    val toOpen = mutable.Stack[(Point, Set[Point])]((start, Set(start)))
    while (toOpen.nonEmpty) {
      val (at, seen) = toOpen.pop()
      if (at == end) {
        if (seen.size > longestPath) {
          longestPath = seen.size
        }
      } else {
        val ns = grid(at.y)(at.x) match {
          case '.' => at.neighbours.filter { pt =>
            !seen.contains(pt) && pt.x >= 0 && pt.y >= 0 && pt.y < grid.length && pt.x < grid(pt.y).length && grid(pt.y)(pt.x) != '#'
          }
          case '>' => List(at.copy(x = at.x + 1)).filter { pt =>
            !seen.contains(pt) && grid(pt.y)(pt.x) == '.'
          }
          case '<' => List(at.copy(x = at.x - 1)).filter { pt =>
            !seen.contains(pt) && grid(pt.y)(pt.x) == '.'
          }
          case '^' => List(at.copy(y = at.y - 1)).filter { pt =>
            !seen.contains(pt) && grid(pt.y)(pt.x) == '.'
          }
          case 'v' => List(at.copy(y = at.y + 1)).filter { pt =>
            !seen.contains(pt) && grid(pt.y)(pt.x) == '.'
          }
        }
        toOpen.pushAll(ns.map { n => (n, seen + n) })
      }
    }
    longestPath - 1
  }

  private[this] def go2(grid: Array[Array[Char]], start: Point, end: Point): Int = {
    var longestPath = Int.MinValue
    val toOpen = mutable.Stack[(Point, Set[Point])]((start, Set(start)))
    while (toOpen.nonEmpty) {
      val (at, seen) = toOpen.pop()
      if (at == end) {
        if (seen.size > longestPath) {
          longestPath = seen.size
          println("Longest so far " + longestPath)
        }
      } else {
        val ns = at.neighbours.filter { pt =>
          !seen.contains(pt) && pt.x >= 0 && pt.y >= 0 && pt.y < grid.length && pt.x < grid(pt.y).length && grid(pt.y)(pt.x) != '#'
        }
        toOpen.pushAll(ns.map { n => (n, seen + n) })
      }
    }
    longestPath - 1
  }
}

object Day23Main extends Day23

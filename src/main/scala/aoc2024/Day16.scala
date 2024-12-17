package aoc2024

import aoc.Direction.Direction
import aoc.{Direction, NewDay, Point}

import scala.collection.mutable

class Day16 extends NewDay(2024, 16) {
  part(1) {
    test(
      """###############
        |#.......#....E#
        |#.#.###.#.###.#
        |#.....#.#...#.#
        |#.###.#####.#.#
        |#.#.#.......#.#
        |#.#.#####.###.#
        |#...........#.#
        |###.#.#####.#.#
        |#...#.....#.#.#
        |#.#.#.###.#.#.#
        |#.....#...#.#.#
        |#.###.#.#.#.#.#
        |#S..#.....#...#
        |###############""".stripMargin -> 7036)
    test(
      """#################
        |#...#...#...#..E#
        |#.#.#.#.#.#.#.#.#
        |#.#.#.#...#...#.#
        |#.#.#.#.###.#.#.#
        |#...#.#.#.....#.#
        |#.#.#.#.#.#####.#
        |#.#...#.#.#.....#
        |#.#.#####.#.###.#
        |#.#.#.......#...#
        |#.#.###.#####.###
        |#.#.#...#.....#.#
        |#.#.#.#####.###.#
        |#.#.#.........#.#
        |#.#.#.#########.#
        |#S#.............#
        |#################""".stripMargin -> 11048)
    execute { ls =>
      val grid = ls.map(_.toCharArray)
      val start = findInGrid(grid, 'S')
      val goal = findInGrid(grid, 'E')

      val s = (start, Direction.East, 0L)
      val visited = mutable.Map[(Point, Direction), Long]()
      val toVisit = mutable.Queue(s)
      var score = Long.MaxValue
      while (toVisit.nonEmpty) {
        val pos = toVisit.dequeue()
        if (pos._1 == goal) {
          if (pos._3 < score) {
            score = pos._3
          }
        }
        if (pos._3 <= score && pos._1 != goal && (!visited.contains((pos._1, pos._2)) || visited((pos._1, pos._2)) > pos._3)) {
          visited((pos._1, pos._2)) = pos._3
          val l90 = pos._2 match {
            case Direction.North => Direction.West
            case Direction.West => Direction.South
            case Direction.South => Direction.East
            case Direction.East => Direction.North
          }
          val r90 = pos._2 match {
            case Direction.North => Direction.East
            case Direction.West => Direction.North
            case Direction.South => Direction.West
            case Direction.East => Direction.South
          }
          val nx = pos._1.go(pos._2)
          if (grid(nx.y)(nx.x) != '#') {
            toVisit.enqueue((pos._1.go(pos._2), pos._2, pos._3 + 1L))
          }
          toVisit.enqueue((pos._1, l90, pos._3 + 1000L))
          toVisit.enqueue((pos._1, r90, pos._3 + 1000L))
        }
      }
      score

    }
  }

  part(2) {
    test(
      """###############
        |#.......#....E#
        |#.#.###.#.###.#
        |#.....#.#...#.#
        |#.###.#####.#.#
        |#.#.#.......#.#
        |#.#.#####.###.#
        |#...........#.#
        |###.#.#####.#.#
        |#...#.....#.#.#
        |#.#.#.###.#.#.#
        |#.....#...#.#.#
        |#.###.#.#.#.#.#
        |#S..#.....#...#
        |###############""".stripMargin -> 45)
    test(
      """#################
        |#...#...#...#..E#
        |#.#.#.#.#.#.#.#.#
        |#.#.#.#...#...#.#
        |#.#.#.#.###.#.#.#
        |#...#.#.#.....#.#
        |#.#.#.#.#.#####.#
        |#.#...#.#.#.....#
        |#.#.#####.#.###.#
        |#.#.#.......#...#
        |#.#.###.#####.###
        |#.#.#...#.....#.#
        |#.#.#.#####.###.#
        |#.#.#.........#.#
        |#.#.#.#########.#
        |#S#.............#
        |#################""".stripMargin -> 64)
    execute { ls =>
      val grid = ls.map(_.toCharArray)
      val start = findInGrid(grid, 'S')
      val goal = findInGrid(grid, 'E')

      val s = (start, Direction.East, 0L, List[Point](start))
      val visited = mutable.Map[(Point, Direction), Long]()
      val toVisit = mutable.Queue(s)
      var score = Long.MaxValue
      val allRoutes = mutable.ListBuffer[(Point, Direction, Long, List[Point])]()
      while (toVisit.nonEmpty) {
        val pos = toVisit.dequeue()
        if (pos._1 == goal) {
          if (pos._3 <= score) {
            score = pos._3
            allRoutes += pos
          }
        }
        if (pos._3 <= score && pos._1 != goal && (!visited.contains((pos._1, pos._2)) || visited((pos._1, pos._2)) >= pos._3)) {
          visited((pos._1, pos._2)) = pos._3
          val l90 = pos._2 match {
            case Direction.North => Direction.West
            case Direction.West => Direction.South
            case Direction.South => Direction.East
            case Direction.East => Direction.North
          }
          val r90 = pos._2 match {
            case Direction.North => Direction.East
            case Direction.West => Direction.North
            case Direction.South => Direction.West
            case Direction.East => Direction.South
          }
          val nx = pos._1.go(pos._2)
          if (grid(nx.y)(nx.x) != '#') {
            toVisit.enqueue((pos._1.go(pos._2), pos._2, pos._3 + 1L, pos._4 :+ pos._1.go(pos._2)))
          }
          toVisit.enqueue((pos._1, l90, pos._3 + 1000L, pos._4))
          toVisit.enqueue((pos._1, r90, pos._3 + 1000L, pos._4))
        }
      }
      allRoutes.filter { _._3 == score }.flatMap { _._4 }.toSet.size
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

object Day16Main extends Day16

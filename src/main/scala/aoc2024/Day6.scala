package aoc2024

import aoc.Direction._
import aoc.{Direction, Point}

import scala.collection.mutable

class Day6 extends aoc.NewDay(2024, 6) {
  part(1) {
    test(
      """....#.....
        |.........#
        |..........
        |..#.......
        |.......#..
        |..........
        |.#..^.....
        |........#.
        |#.........
        |......#...""".stripMargin -> 41)

    execute { ls =>
      val grid = ls.map(_.toCharArray)
      var position: Point = null
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          if (grid(c)(r) == '^') {
            position = Point(r, c)
          }
        }
      }
      val visited: mutable.Set[Point] = mutable.Set()
      var direction = Direction.North
      while (position.y >= 0 && position.y < grid.length && position.x >= 0 && position.x < grid(position.y).length) {
        visited += position
        val next = position.go(direction)
        if (next.y < 0 || next.y >= grid.length || next.x < 0 || next.x >= grid(next.y).length || grid(next.y)(next.x) != '#') {
          position = next
        } else {
          direction = direction match {
            case North => East
            case East => South
            case South => West
            case West => North
          }
        }
      }
      visited.size
    }
  }

  part(2) {
    test(
      """....#.....
        |.........#
        |..........
        |..#.......
        |.......#..
        |..........
        |.#..^.....
        |........#.
        |#.........
        |......#...""".stripMargin -> 6)

    execute { ls =>
      val grid = ls.map(_.toCharArray)
      var position: Point = null
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          if (grid(c)(r) == '^') {
            position = Point(r, c)
          }
        }
      }

      var loops = 0
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          // Can only loop by adding a block
          if (grid(c)(r) == '.') {
            grid(c)(r) = '#'
            val thisLoops = checkLoop(grid, position)
            if (thisLoops) {
              loops = loops + 1
            }
            grid(c)(r) = '.'
          }
        }
      }

      loops
    }

    def checkLoop(grid: Array[Array[Char]], start: Point): Boolean = {
      val visited: mutable.Set[(Point, Direction.Value)] = mutable.Set()
      var direction = Direction.North
      var position = start

      while (!visited.contains((position, direction)) && position.y >= 0 && position.y < grid.length && position.x >= 0 && position.x < grid(position.y).length) {
        visited.add((position, direction))
        val next = position.go(direction)
        if (next.y < 0 || next.y >= grid.length || next.x < 0 || next.x >= grid(next.y).length) {
          position = next
        } else if (grid(next.y)(next.x) != '#') {
          position = next
        } else {
          direction = rotate(direction)
        }
      }
      visited.contains((position, direction))
    }

    def rotate(d: Direction): Direction = {
      d match {
        case North => East
        case East => South
        case South => West
        case West => North
      }
    }

  }
}

object Day6Main extends Day6

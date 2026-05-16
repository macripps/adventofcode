package aoc2017

import aoc.{NewDay, Direction, Point}

import scala.collection.mutable

class Day22 extends NewDay(2017, 22) {
  part(1) {
    execute { in =>
      val turn = Map[(Direction.Direction, Boolean), Direction.Direction](
        (Direction.North, true) -> Direction.East,
        (Direction.North, false) -> Direction.West,
        (Direction.East, true) -> Direction.South,
        (Direction.East, false) -> Direction.North,
        (Direction.South, true) -> Direction.West,
        (Direction.South, false) -> Direction.East,
        (Direction.West, true) -> Direction.North,
        (Direction.West, false) -> Direction.South,
      )
      val grid = mutable.Map[Point, Boolean]()
      var c = Point(in.length / 2, in(0).length / 2)
      var d = Direction.North
      println("Center: " + c)
      in.indices.foreach { y =>
        in(y).indices.foreach { x =>
          grid(Point(x, y)) = in(y)(x) == '#'
        }
      }
      var infections = 0
      (1 to 10000).foreach { _ =>
        val current = grid.getOrElse(c, false)
        d = turn((d, current))
        if (!current) {
          infections = infections + 1
          grid(c) = true
        } else {
          grid(c) = false
        }
        d match {
          case Direction.North => c = c.copy(y = c.y - 1)
          case Direction.East => c = c.copy(x = c.x + 1)
          case Direction.South => c = c.copy(y = c.y + 1)
          case Direction.West => c = c.copy(x = c.x - 1)
        }
      }
      infections.toString
    }
  }

  part(2) {
    execute { in =>
      val turn = Map[(Direction.Direction, Int), Direction.Direction](
        (Direction.North, 0) -> Direction.West,
        (Direction.East, 0) -> Direction.North,
        (Direction.South, 0) -> Direction.East,
        (Direction.West, 0) -> Direction.South,
        (Direction.North, 1) -> Direction.North,
        (Direction.East, 1) -> Direction.East,
        (Direction.South, 1) -> Direction.South,
        (Direction.West, 1) -> Direction.West,
        (Direction.North, 2) -> Direction.East,
        (Direction.East, 2) -> Direction.South,
        (Direction.South, 2) -> Direction.West,
        (Direction.West, 2) -> Direction.North,
        (Direction.North, 3) -> Direction.South,
        (Direction.East, 3) -> Direction.West,
        (Direction.South, 3) -> Direction.North,
        (Direction.West, 3) -> Direction.East,
      )
      val grid = mutable.Map[Point, Int]()
      var c = Point(in.length / 2, in(0).length / 2)
      var d = Direction.North
      println("Center: " + c)
      in.indices.foreach { y =>
        in(y).indices.foreach { x =>
          grid(Point(x, y)) = if (in(y)(x) == '#') 2 else 0
        }
      }
      var infections = 0
      (1 to 10_000_000).foreach { _ =>
        val current = grid.getOrElse(c, 0)
        d = turn((d, current))
        if (current == 1) {
          infections = infections + 1
        }
        grid(c) = (current + 1) % 4
        d match {
          case Direction.North => c = c.copy(y = c.y - 1)
          case Direction.East => c = c.copy(x = c.x + 1)
          case Direction.South => c = c.copy(y = c.y + 1)
          case Direction.West => c = c.copy(x = c.x - 1)
        }
      }
      infections.toString
    }
  }
}

object Day22Main extends Day22

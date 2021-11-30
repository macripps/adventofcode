package aoc2017

import aoc.{Day, Direction, Point}

class Day19 extends Day(2017, 19) {
  override def part1(input: Array[String]): String = {
    val grid = input.map(_.toCharArray)
    val start = Point(grid(0).indexOf('|'), 0)

    var direction = Direction.South
    var p = start
    var out = ""
    while (grid(p.y)(p.x) != ' ') {
      direction match {
        case Direction.South => p = p.copy(y = p.y + 1)
        case Direction.East => p = p.copy(x = p.x + 1)
        case Direction.North => p = p.copy(y = p.y - 1)
        case Direction.West => p = p.copy(x = p.x - 1)
      }
      val chr = grid(p.y)(p.x)
      if (chr >= 'A' && chr <= 'Z') {
        out = out + chr
      } else if (chr == '+') {
        direction match {
          case Direction.South | Direction.North =>
            if (grid(p.y)(p.x - 1) == '-') {
              direction = Direction.West
            } else if (grid(p.y)(p.x + 1) == '-') {
              direction = Direction.East
            }
          case Direction.East | Direction.West =>
            if (grid(p.y - 1)(p.x) == '|') {
              direction = Direction.North
            } else if (grid(p.y + 1)(p.x) == '|') {
              direction = Direction.South
            }
        }
      }
    }
    out
  }

  override def part2(input: Array[String]): String = {
    val grid = input.map(_.toCharArray)
    val start = Point(grid(0).indexOf('|'), 0)

    var direction = Direction.South
    var p = start
    var steps = 0
    while (grid(p.y)(p.x) != ' ') {
      direction match {
        case Direction.South => p = p.copy(y = p.y + 1)
        case Direction.East => p = p.copy(x = p.x + 1)
        case Direction.North => p = p.copy(y = p.y - 1)
        case Direction.West => p = p.copy(x = p.x - 1)
      }
      steps = steps + 1
      val chr = grid(p.y)(p.x)
      if (chr == '+') {
        direction match {
          case Direction.South | Direction.North =>
            if (grid(p.y)(p.x - 1) == '-') {
              direction = Direction.West
            } else if (grid(p.y)(p.x + 1) == '-') {
              direction = Direction.East
            }
          case Direction.East | Direction.West =>
            if (grid(p.y - 1)(p.x) == '|') {
              direction = Direction.North
            } else if (grid(p.y + 1)(p.x) == '|') {
              direction = Direction.South
            }
        }
      }
    }
    steps.toString
  }
}

object Day19 {
  def apply() = new Day19()
}

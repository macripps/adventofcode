package aoc2023

import aoc.{Direction, LongPoint, NewDay, Point}

class Day18 extends NewDay(2023, 18) {
  part(1) {
    test {
      """R 6 (#70c710)
        |D 5 (#0dc571)
        |L 2 (#5713f0)
        |D 2 (#d2c081)
        |R 2 (#59c680)
        |D 2 (#411b91)
        |L 5 (#8ceee2)
        |U 2 (#caa173)
        |L 1 (#1b58a2)
        |U 2 (#caa171)
        |R 2 (#7807d2)
        |U 3 (#a77fa3)
        |L 2 (#015232)
        |U 2 (#7a21e3)""".stripMargin -> 62
    }

    execute { input =>
      val instructions = input.map { inst =>
        val Array(dir, num, _) = inst.split(' ')
        val direction = dir match {
          case "U" => Direction.South
          case "R" => Direction.East
          case "D" => Direction.North
          case "L" => Direction.West
        }
        (direction, num.toLong)
      }
      area(instructions)
    }
  }

  part(2) {
    test {
      """R 6 (#70c710)
        |D 5 (#0dc571)
        |L 2 (#5713f0)
        |D 2 (#d2c081)
        |R 2 (#59c680)
        |D 2 (#411b91)
        |L 5 (#8ceee2)
        |U 2 (#caa173)
        |L 1 (#1b58a2)
        |U 2 (#caa171)
        |R 2 (#7807d2)
        |U 3 (#a77fa3)
        |L 2 (#015232)
        |U 2 (#7a21e3)""".stripMargin -> 952408144115L
    }
    execute { input =>
      val instructions = input.map { inst =>
        val Array(_, _, col) = inst.split(' ')
        val len = java.lang.Long.parseLong(col.drop(2).dropRight(2), 16)
        val direction = col.dropRight(1).takeRight(1) match {
          case "0" => Direction.East
          case "1" => Direction.North
          case "2" => Direction.West
          case "3" => Direction.South
        }
        (direction, len)
      }

      area(instructions)
    }
  }


  private[this] def area(instructions: Array[(Direction.Value, Long)]) = {
    var current = LongPoint(0L, 0L)
    var discrim = 0L
    var len = 0L
    instructions.foreach { case (direction, num) =>
      val last = current
      current = current.go(direction, num)
      len = len + num
      discrim = discrim + (current.x - last.x) * (current.y + last.y)
    }
    (discrim / 2) + (len / 2) + 1
  }

}

object Day18Main extends Day18

package aoc2016

import aoc.{Day, Direction, Point}

import scala.collection.mutable

class Day1 extends Day(2016, 1) {
  lazy val commands = input.head.split(", ")
  override def part1: String = {
    val result = commands.foldLeft((Direction.North, Point(0, 0))){ case (state, command) =>
      val newDirection: Direction.Direction = command.charAt(0) match {
        case 'L' =>
          state._1 match {
            case Direction.North => Direction.West
            case Direction.West => Direction.South
            case Direction.South => Direction.East
            case Direction.East => Direction.North
          }
        case 'R' => {
          state._1 match {
            case Direction.North => Direction.East
            case Direction.West => Direction.North
            case Direction.South => Direction.West
            case Direction.East => Direction.South
          }
        }
      }
      val distance = command.substring(1).toInt
      val newPosition = newDirection match {
        case Direction.North => Point(state._2.x, state._2.y + distance)
        case Direction.East => Point(state._2.x + distance, state._2.y)
        case Direction.South => Point(state._2.x, state._2.y - distance)
        case Direction.West => Point(state._2.x - distance, state._2.y)
      }
      (newDirection, newPosition)
    }
    result._2.manhattanDistance.toString
  }

  override def part2: String = {
    val visited = mutable.Set[Point]()
    var firstVisitedPosition: Option[Point] = None
    var position = Point(0, 0)
    var direction = Direction.North

    var cs = commands
    while (!cs.isEmpty) {
      val command = cs.head
      val newDirection = command.charAt(0) match {
        case 'L' =>
          direction match {
            case Direction.North => Direction.West
            case Direction.West => Direction.South
            case Direction.South => Direction.East
            case Direction.East => Direction.North
          }
        case 'R' => {
          direction match {
            case Direction.North => Direction.East
            case Direction.West => Direction.North
            case Direction.South => Direction.West
            case Direction.East => Direction.South
          }
        }
      }
      val distance = command.substring(1).toInt
      val newPosition = newDirection match {
        case Direction.North => Point(position.x, position.y + distance)
        case Direction.East => Point(position.x + distance, position.y)
        case Direction.South => Point(position.x, position.y - distance)
        case Direction.West => Point(position.x - distance, position.y)
      }

      (math.min(position.x, newPosition.x) to math.max(position.x, newPosition.x)).foreach { x =>
        (math.min(position.y, newPosition.y) to math.max(position.y, newPosition.y)).foreach { y =>
          val p = Point(x, y)
          if (p != position && visited.contains(p) && !firstVisitedPosition.isDefined) {
            firstVisitedPosition = Some(p)
          }
          visited.add(p)
        }
      }

      position = newPosition
      direction = newDirection
      cs = cs.tail
    }
    firstVisitedPosition.map(_.manhattanDistance).getOrElse(0).toString
  }
}

object Day1 {
  def apply() = new Day1()
}

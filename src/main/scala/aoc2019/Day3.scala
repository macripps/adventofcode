package aoc2019

import aoc.{NewDay, Point}

class Day3 extends NewDay(2019, 3) {
  part(1) {
    test(
      """R8,U5,L5,D3
        |U7,R6,D4,L4""".stripMargin -> 6
    )
    execute { in =>
      var grid = Set[Point]()
      var current = Point(0, 0)
      in.head.split(',').foreach { c =>
        val delta = direction(c.charAt(0))
        val amount = c.tail.toInt
        val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
        while (current != destination) {
          current = Point(current.x + delta._1, current.y + delta._2)
          grid = grid + current
        }
      }
      var nearestDistance = Int.MaxValue
      current = Point(0, 0)
      in.tail.head.split(',').foreach { c =>
        val delta = direction(c.charAt(0))
        val amount = c.tail.toInt
        val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
        while (current != destination) {
          current = Point(current.x + delta._1, current.y + delta._2)
          if (grid.contains(current)) {
            nearestDistance = math.min(current.manhattanDistanceTo(Point(0, 0)), nearestDistance)
          }
        }
      }
      nearestDistance
    }
  }

  part(2) {
    test(
      """R8,U5,L5,D3
        |U7,R6,D4,L4""".stripMargin -> 30
    )
    execute { in =>
      var grid = Map[Point, Int]()
      var current = Point(0, 0)
      var steps = 0
      in.head.split(',').foreach { c =>
        val delta = direction(c.charAt(0))
        val amount = c.tail.toInt
        val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
        while (current != destination) {
          current = Point(current.x + delta._1, current.y + delta._2)
          steps = steps + 1
          if (!grid.contains(current)) grid = grid + (current -> steps)
        }
      }
      var nearestDistance = Int.MaxValue
      current = Point(0, 0)
      steps = 0
      in.tail.head.split(',').foreach { c =>
        val delta = direction(c.charAt(0))
        val amount = c.tail.toInt
        val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
        while (current != destination) {
          current = Point(current.x + delta._1, current.y + delta._2)
          steps = steps + 1
          if (grid.contains(current)) {
            nearestDistance = math.min(grid(current) + steps, nearestDistance)
          }
        }
      }
      nearestDistance
    }
  }

  private def direction(c: Char): (Int, Int) = c match {
    case 'U' => (0, 1)
    case 'D' => (0, -1)
    case 'L' => (-1, 0)
    case 'R' => (1, 0)
  }
}

object Day3Main extends Day3

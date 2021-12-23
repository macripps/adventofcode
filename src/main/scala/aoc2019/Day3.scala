package aoc2019

import aoc.{Day, Point}

class Day3 extends Day(2019, 3) {
  override def part1(input: Array[String]): String = {
    var grid = Set[Point]()
    val wire1 = input.head.split(',')
    var current = Point(0,0)
    wire1.foreach { c =>
      val delta = c.charAt(0) match {
        case 'U' => (0, 1)
        case 'D' => (0, -1)
        case 'L' => (-1, 0)
        case 'R' => (1, 0)
      }
      val amount = c.tail.toInt
      val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
      while (current != destination) {
        current = Point(current.x + delta._1, current.y + delta._2)
        grid = grid + current
      }
    }
    var nearestDistance = Int.MaxValue
    val wire2 = input.tail.head.split(',')
    current = Point(0,0)
    wire2.foreach { c =>
      val delta = c.charAt(0) match {
        case 'U' => (0, 1)
        case 'D' => (0, -1)
        case 'L' => (-1, 0)
        case 'R' => (1, 0)
      }
      val amount = c.tail.toInt
      val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
      while (current != destination) {
        current = Point(current.x + delta._1, current.y + delta._2)
        if (grid.contains(current)) {
          nearestDistance = math.min(current.manhattanDistanceTo(Point(0,0)), nearestDistance)
        }
      }
    }
    nearestDistance.toString
  }

  override def part2(input: Array[String]): String = {
    var grid = Map[Point, Int]()
    val wire1 = input.head.split(',')
    var current = Point(0,0)
    var steps = 0
    wire1.foreach { c =>
      val delta = c.charAt(0) match {
        case 'U' => (0, 1)
        case 'D' => (0, -1)
        case 'L' => (-1, 0)
        case 'R' => (1, 0)
      }
      val amount = c.tail.toInt
      val destination = Point(current.x + delta._1 * amount, current.y + delta._2 * amount)
      while (current != destination) {
        current = Point(current.x + delta._1, current.y + delta._2)
        steps = steps + 1
        grid = grid + (current -> steps)
      }
    }
    var nearestDistance = Int.MaxValue
    val wire2 = input.tail.head.split(',')
    current = Point(0,0)
    steps = 0
    wire2.foreach { c =>
      val delta = c.charAt(0) match {
        case 'U' => (0, 1)
        case 'D' => (0, -1)
        case 'L' => (-1, 0)
        case 'R' => (1, 0)
      }
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
    nearestDistance.toString
  }
}

object Day3 {
  def apply() = new Day3

  val example1 =
    """
      |R8,U5,L5,D3
      |U7,R6,D4,L4""".stripMargin.split("\n")
}

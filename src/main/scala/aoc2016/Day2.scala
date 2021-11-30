package aoc2016

import aoc.{Day, Point}

class Day2 extends Day(2016, 2) {
  override def part1(input: Array[String]): String = {
    var position = Point(1,1)
    val maxYs = Seq(0, 0, 0)
    val minXs = Seq(0, 0, 0)
    val minYs = Seq(2, 2, 2)
    val maxXs = Seq(2, 2, 2)
    input.map { line =>
      line.foreach {
        case 'U' => position = Point(position.x, math.max(maxYs(position.x), position.y - 1))
        case 'L' => position = Point(math.max(minXs(position.y), position.x - 1), position.y)
        case 'D' => position = Point(position.x, math.min(minYs(position.x), position.y + 1))
        case 'R' => position = Point(math.min(maxXs(position.y), position.x + 1), position.y)
      }
      1 + position.x + (position.y*3)
    }.mkString
  }

  override def part2(input: Array[String]): String = {
    var position = Point(0, 2)
    val maxYs = Seq(2, 1, 0, 1, 2)
    val minXs = Seq(2, 1, 0, 1, 2)
    val minYs = Seq(2, 3, 4, 3, 2)
    val maxXs = Seq(2, 3, 4, 3, 2)
    input.map { line =>
      line.foreach {
        case 'U' => position = Point(position.x, math.max(maxYs(position.x), position.y - 1))
        case 'L' => position = Point(math.max(minXs(position.y), position.x - 1), position.y)
        case 'D' => position = Point(position.x, math.min(minYs(position.x), position.y + 1))
        case 'R' => position = Point(math.min(maxXs(position.y), position.x + 1), position.y)
      }
      position match {
        case Point(2, 0) => '1'
        case Point(1, 1) => '2'
        case Point(2, 1) => '3'
        case Point(3, 1) => '4'
        case Point(0, 2) => '5'
        case Point(1, 2) => '6'
        case Point(2, 2) => '7'
        case Point(3, 2) => '8'
        case Point(4, 2) => '9'
        case Point(1, 3) => 'A'
        case Point(2, 3) => 'B'
        case Point(3, 3) => 'C'
        case Point(2, 4) => 'D'
        case _ => '?'
      }
    }.mkString
  }
}

object Day2 {
  def apply() = new Day2()
}

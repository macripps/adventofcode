package aoc2015

import aoc.Day

import scala.collection.mutable

class Day3 extends Day(2015, 3) {
  override def part1: String = {
    val visited = mutable.Set[(Int, Int)]((0, 0))
    var position = (0, 0)
    input(0).foreach { c =>
      c match {
        case 'v' => position = (position._1, position._2 - 1)
        case '<' => position = (position._1 - 1, position._2)
        case '>' => position = (position._1 + 1, position._2)
        case '^' => position = (position._1, position._2 + 1)
      }
      visited += position
    }
    visited.size.toString
  }

  override def part2: String = {
    val visited = mutable.Set[(Int, Int)]((0, 0))
    var position1 = (0, 0)
    var position2 = (0, 0)
    input(0).grouped(2).foreach { c =>
      c(0) match {
        case 'v' => position1 = (position1._1, position1._2 - 1)
        case '<' => position1 = (position1._1 - 1, position1._2)
        case '>' => position1 = (position1._1 + 1, position1._2)
        case '^' => position1 = (position1._1, position1._2 + 1)
      }
      visited += position1
      c(1) match {
        case 'v' => position2 = (position2._1, position2._2 - 1)
        case '<' => position2 = (position2._1 - 1, position2._2)
        case '>' => position2 = (position2._1 + 1, position2._2)
        case '^' => position2 = (position2._1, position2._2 + 1)
      }
      visited += position2
    }
    visited.size.toString
  }
}

object Day3 {
  def apply() = new Day3()
}

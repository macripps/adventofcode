package aoc2017

import aoc.{Day, HexPoint}

class Day11 extends Day(2017, 11) {
  override def part1: String = {
    var p = HexPoint(0, 0, 0)
    val dirs = input.head.split(",")
    dirs.foreach {
      case "s" => p = p.s
      case "se" => p = p.se
      case "sw" => p = p.sw
      case "ne" => p = p.ne
      case "n" => p = p.n
      case "nw" => p = p.nw
    }
    p.manhattanDistanceTo(HexPoint(0, 0, 0)).toString
  }

  override def part2: String = {
    var max = 0
    var p = HexPoint(0, 0, 0)
    val dirs = input.head.split(",")
    dirs.foreach { x =>
      x match {
        case "s" => p = p.s
        case "se" => p = p.se
        case "sw" => p = p.sw
        case "ne" => p = p.ne
        case "n" => p = p.n
        case "nw" => p = p.nw
      }
      val d = p.manhattanDistanceTo(HexPoint(0, 0, 0))
      println(x + ": " + d)
      if (d > max) {
        max = d
      }
    }
    max.toString
  }
}

object Day11 {
  def apply() = new Day11()
}

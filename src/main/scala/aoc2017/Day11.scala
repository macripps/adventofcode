package aoc2017

import aoc.{NewDay, HexPoint}

class Day11 extends NewDay(2017, 11) {
  part(1) {
    execute { in =>
      var p = HexPoint(0, 0, 0)
      val dirs = in.head.split(",")
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
  }

  part(2) {
    execute { in =>
      var max = 0
      var p = HexPoint(0, 0, 0)
      val dirs = in.head.split(",")
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
}

object Day11Main extends Day11

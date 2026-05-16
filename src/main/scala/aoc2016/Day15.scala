package aoc2016

import aoc.NewDay

import scala.util.matching.Regex

class Day15 extends NewDay(2016, 15) {

  import Day15._

  part(1) {
    execute { in =>
      val pairs = in.map { case Disc(id: String, positions: String, position: String) => ((positions.toInt - (position.toInt + id.toInt)) % positions.toInt, positions.toInt) }
      aoc.chineseRemainerTheorem(pairs)._1.toString
    }
  }

  part(2) {
    execute { in =>
      val pairs = in.map { case Disc(id: String, positions: String, position: String) => ((positions.toInt - (position.toInt + id.toInt)) % positions.toInt, positions.toInt) } :+ (11 - (in.length + 1), 11)
      aoc.chineseRemainerTheorem(pairs)._1.toString
    }
  }
}

object Day15 {
  val Disc: Regex = raw"Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+).".r
}

object Day15Main extends Day15

package aoc2016

import aoc.Day

import scala.util.matching.Regex

class Day15 extends Day(2016, 15) {

  import Day15._

  override def part1(input: Array[String]): String = {
    val pairs = input.map { case Disc(id: String, positions: String, position: String) => ((positions.toInt - (position.toInt + id.toInt)) % positions.toInt, positions.toInt) }
    aoc.chineseRemainerTheorem(pairs)._1.toString
  }

  override def part2(input: Array[String]): String = {
    val pairs = input.map { case Disc(id: String, positions: String, position: String) => ((positions.toInt - (position.toInt + id.toInt)) % positions.toInt, positions.toInt) } :+ (11 - (input.length + 1), 11)
    aoc.chineseRemainerTheorem(pairs)._1.toString
  }
}

object Day15 {
  def apply() = new Day15()

  val Disc: Regex = raw"Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+).".r
}

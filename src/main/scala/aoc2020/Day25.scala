package aoc2020

import aoc.Day

class Day25 extends Day(2020, 25) {
  override def part1: String = {
    var card = 1L
    val cardTarget = input(0).toLong
    var cardSize = 1
    var doorKey = 1L
    while (card != cardTarget) {
      card = (card * 7L) % 20201227L
      doorKey = (doorKey * 12721030L) % 20201227L
      cardSize = cardSize + 1
    }
    doorKey.toString
  }

  override def part2: String = ""
}

object Day25 {
  def apply() = new Day25()
}

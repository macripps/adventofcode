package aoc2017

import aoc.Day

class Day9 extends Day(2017, 9) {
  override def part1(input: Array[String]): String = {
    val line = input.head
    var score = 0
    var grp = 0
    var c = 0
    var inGarbage = false
    while (c < line.length) {
      line(c) match {
        case '!' => c = c + 1
        case '<' if !inGarbage => inGarbage = true
        case '>' if inGarbage => inGarbage = false
        case '{' if !inGarbage => grp = grp + 1
        case '}' if !inGarbage =>
          score = score + grp
          grp = grp - 1
        case _ =>
      }
      c = c + 1
    }
    score.toString
  }

  override def part2(input: Array[String]): String = {
    val line = input.head
    var garbageChars = 0
    var c = 0
    var inGarbage = false
    while (c < line.length) {
      line(c) match {
        case '!' => c = c + 1
        case '<' if !inGarbage => inGarbage = true
        case '>' if inGarbage => inGarbage = false
        case _ if inGarbage => garbageChars = garbageChars + 1
        case _ =>
      }
      c = c + 1
    }
    garbageChars.toString
  }
}

object Day9 {
  def apply() = new Day9()
}

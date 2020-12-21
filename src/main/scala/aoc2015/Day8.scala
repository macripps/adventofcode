package aoc2015

import aoc.Day
import aoc2015.Day8._

class Day8 extends Day(2015, 8) {
  override def part1: String = {
    (input.map(codeCharacters).sum - input.map(inMemoryCharacters).sum).toString
  }

  override def part2: String = {
    (input.map(escapeCharacters).sum - input.map(codeCharacters).sum).toString
  }
}

object Day8 {
  def apply() = new Day8()

  def codeCharacters(s: String) = s.length

  def inMemoryCharacters(s: String): Int = {
    val u = s.drop(1).dropRight(1)
    var i = 0
    var l = 0
    while (i < u.length) {
      if (u.charAt(i) != '\\') {
        i = i + 1
      } else {
        if (u.charAt(i+1) != 'x') {
          i = i + 2
        } else {
          i = i + 4
        }
      }
      l = l + 1
    }
    l
  }

  def escapeCharacters(s: String): Int = {
    s.length + 2 + s.count{c => c == '\\'} + s.count(c => c == '"')
  }
}

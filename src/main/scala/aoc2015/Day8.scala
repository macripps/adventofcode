package aoc2015

import aoc.NewDay
import aoc2015.Day8._

class Day8 extends NewDay(2015, 8) {
  part(1) {
    execute { in =>
      (in.map(codeCharacters).sum - in.map(inMemoryCharacters).sum).toString
    }
  }

  part(2) {
    execute { in =>
      (in.map(escapeCharacters).sum - in.map(codeCharacters).sum).toString
    }
  }
}

object Day8 {
  def codeCharacters(s: String): Int = s.length

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

object Day8Main extends Day8

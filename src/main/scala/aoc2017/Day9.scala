package aoc2017

import aoc.NewDay

class Day9 extends NewDay(2017, 9) {
  part(1) {
    execute { in =>
      val line = in.head
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
  }

  part(2) {
    execute { in =>
      val line = in.head
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
}

object Day9Main extends Day9

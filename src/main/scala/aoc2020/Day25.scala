package aoc2020

import aoc.NewDay

class Day25 extends NewDay(2020, 25) {
  part(1) {
    execute { in =>
      var card = 1L
      val cardTarget = in(0).toLong
      var cardSize = 1
      var doorKey = 1L
      while (card != cardTarget) {
        card = (card * 7L) % 20201227L
        doorKey = (doorKey * 12721030L) % 20201227L
        cardSize = cardSize + 1
      }
      doorKey.toString
    }
  }

  part(2) {
    execute { _ => "" }
  }
}

object Day25Main extends Day25

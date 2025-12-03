package aoc2025

import aoc.NewDay

class Day3 extends NewDay(2025, 3) {

  part(1) {
    test("""987654321111111
           |811111111111119
           |234234234234278
           |818181911112111""".stripMargin -> 357)
    execute { lines =>
      lines.map { l =>
        val maxL = l.toCharArray.dropRight(1).max
        val maxLAt = l.indexOf(maxL)
        val maxR = l.toCharArray.drop(maxLAt + 1).max
        (10 * (maxL - '0')) + (maxR - '0')
      }.sum
    }
  }

  part(2) {
    test("""987654321111111
           |811111111111119
           |234234234234278
           |818181911112111""".stripMargin -> 3121910778619L)
    execute { lines =>
      lines.map { l =>
        var count = 12
        var score = 0L
        var start = 0
        while (count > 0) {
          count = count - 1
          val max = l.drop(start).dropRight(count).max
          start = l.indexOf(max, start) + 1
          score = (score * 10) + (max - '0')
        }
        score
      }.sum
    }
  }
}

object Day3Main extends Day3

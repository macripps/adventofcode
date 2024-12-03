package aoc2024

import aoc.NewDay

class Day3 extends NewDay(2024, 3) {
  part(1) {
    test("""xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""" -> 161)
    execute { ls =>
      var total = 0L
      ls.foreach { l =>
        var c = 0
        while (c + 4 < l.length) {
          if (l.substring(c, c + 4) == "mul(") {
            c = c + 4
            val g1 = readNum(l, c)
            if (g1.nonEmpty && math.abs(g1.toLong) < 1000) {
              c = c + g1.length
              if (l.charAt(c) == ',') {
                c = c + 1
                val g2 = readNum(l, c)
                c = c + g2.length
                if (l.charAt(c) == ')') {
                  c = c + 1
                  total = total + (g1.toLong * g2.toLong)
                }
              }
            }
          } else {
            c = c + 1
          }
        }
      }
      total
    }
  }

  part(2) {
    test("""xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))""" -> 48)
    execute { ls =>
      var e = true
      var total = 0L
      ls.foreach { l =>
        var c = 0
        while (c + 7 < l.length) {
          if (l.substring(c, c + 4) == "do()") {
            e = true
            c = c + 4
          } else if (l.substring(c, c + 7) == "don't()") {
            e = false
            c = c + 7
          } else if (l.substring(c, c + 4) == "mul(") {
            c = c + 4
            val g1 = readNum(l, c)
            if (g1.nonEmpty && math.abs(g1.toLong) < 1000) {
              c = c + g1.length
              if (l.charAt(c) == ',') {
                c = c + 1
                val g2 = readNum(l, c)
                c = c + g2.length
                if (l.charAt(c) == ')') {
                  c = c + 1
                  if (e) {
                    total = total + (g1.toLong * g2.toLong)
                  }
                }
              }
            }
          } else {
            c = c + 1
          }
        }
      }
      total
    }
  }

  private[this] def readNum(l: String, start: Int): String = {
    var g1 = ""
    var c = start
    val n = l.charAt(c) == '-'
    if (n) c = c + 1
    while (l.charAt(c).isDigit) {
      g1 = g1 + l.charAt(c)
      c = c + 1
    }
    (if (n) "-" else "") + g1
  }
}

object Day3Main extends Day3

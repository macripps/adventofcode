package aoc2025

import aoc.NewDay

class Day6 extends NewDay(2025, 6) {

  part(1) {
    test("""123 328  51 64
           | 45 64  387 23
           |  6 98  215 314
           |*   +   *   +  """.stripMargin -> 4277556L)
    execute { ls =>
      var ns = Array.fill(ls.length-1)("")
      var op = '_'
      var idx = 0
      var result = 0L
      val len = ls.map(_.length).max
      val padded = ls.map{x: String => x.padTo(len + 1, ' ')}
      while (idx <= len) {
        if (padded.forall(_.charAt(idx) == ' ')) {
          op match {
            case '*' => result = result + ns.map(_.toLong).product
            case '+' => result = result + ns.map(_.toLong).sum
          }
          ns = Array.fill(padded.length-1)("")
          op = '_'
        } else {
          padded.indices.dropRight(1).foreach { n =>
            if (padded(n).charAt(idx) != ' ') {
              ns(n) += padded(n).charAt(idx)
            }
          }
          if (padded(padded.length-1).charAt(idx) != ' ') {
            op = padded(padded.length-1).charAt(idx)
          }
        }
        idx = idx + 1
      }
      result
    }
  }

  part(2) {
    test("""123 328  51 64
           | 45 64  387 23
           |  6 98  215 314
           |*   +   *   +  """.stripMargin -> 3263827L)
    execute { ls =>
      var start = 0
      var idx = 0
      var result = 0L
      val len = ls.map(_.length).max
      val padded = ls.map{x: String => x.padTo(len + 1, ' ')}
      while (idx <= len) {
        while (padded.exists(_.charAt(idx) != ' ')) {
          idx = idx + 1
        }
        var op = '_'
        val ns = Array.fill(idx - start)("")
        start.until(idx).foreach { id =>
          padded.indices.dropRight(1).foreach { l =>
            if (padded(l)(id) != ' ') {
              ns(id - start) = ns(id - start) + padded(l)(id)
            }
            if (padded.last(id) != ' ') {
              op = padded.last(id)
            }
          }
        }
        val r = op match {
          case '+' => ns.map(_.toLong).sum
          case '*' => ns.map(_.toLong).product
        }
        if (debug()) {
          debug(ns.mkString(" "+op+ " ") + " = " + r)
        }
        result = result + r
        idx = idx + 1
        start = idx
      }
      result
    }
  }
}

object Day6Main extends Day6
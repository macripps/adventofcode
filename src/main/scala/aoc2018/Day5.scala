package aoc2018

import aoc.NewDay

class Day5 extends NewDay(2018, 5) {
  part(1) {
    execute { in =>
      collapse(in.head).length.toString
    }
  }

  var rx = "aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ"
  rx = rx + "|" + rx.reverse

  def collapse(polymer: String): String = {
    var p = polymer
    var o = ""
    while (p != o) {
      o = p
      p = p.replaceAll(rx, "")
    }
    p
  }

  def other(c: Char): Char = {
    if (c >= 'A' && c <= 'Z') {
      (c + 0x20).toChar
    } else {
      (c - 0x20).toChar
    }
  }

  part(2) {
    execute { in =>
      val polymer = in.head
      // var polymer = "dabAcCaCBAcCcaDA"
      ('A' to 'Z').map { c =>
        polymer.filter { x => x != c && x != other(c) }.mkString
      }.toSet[String].map { r =>
        val o = collapse(r)
        println(o.length)
        o.length
      }.min.toString
    }
  }
}

object Day5Main extends Day5

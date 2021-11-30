package aoc2020

import aoc.Day
import Day18._

class Day18 extends Day(2020, 18) {
  override def part1(input: Array[String]): String = {
    input.map(_.replace(" ", "")).map(parse).sum.toString
  }

  override def part2(input: Array[String]): String = {
    input.map(_.replace(" ", "")).map(parse2).sum.toString
  }
}

object Day18 {
  def apply() = new Day18()

  def parse(line: String): Long = {
    line.toLongOption match {
      case Some(x) => x
      case None => line match {
        case Paren(x) => parse(x)
        case Add(x, y) => parse(x) + parse(y)
        case Product(x, y) => parse(x) * parse(y)
      }
    }
  }

  def parse2(line: String): Long = {
    line.toLongOption match {
      case Some(x) => x
      case None => line match {
        case Paren(x) => parse2(x)
        case Product2(x, y) => parse2(x) * parse2(y)
        case Add2(x, y) => parse2(x) + parse2(y)
      }
    }
  }

  object Paren {
    def unapply(x: String): Option[String] = {
      if (x.charAt(0) == '(') {
        var everOut = false
        var d = 1
        var i = 1
        while (i < x.length - 1) {
          if (x.charAt(i) == '(') {
            d = d + 1
          } else if (x.charAt(i) == ')') {
            d = d - 1
          }
          if (d == 0) {
            everOut = true
          }
          i = i + 1
        }
        if (everOut) None else Some(x.drop(1).dropRight(1))
      } else None
    }
  }

  object Add {
    def unapply(x: String): Option[(String, String)] = {
      val plusIdx = find(x, '+')
      val timesIdx = find(x, '*')
      if (plusIdx > timesIdx) {
        Some(x.substring(0, plusIdx), x.substring(plusIdx + 1))
      } else {
        None
      }
    }
  }

  object Add2 {
    def unapply(x: String): Option[(String, String)] = {
        val pIdx = find(x, '+')
        if (pIdx > 0) {
          Some(x.substring(0, pIdx), x.substring(pIdx + 1))
        } else None
      }
  }

  object Product {
    def unapply(x: String): Option[(String, String)] = {
      val plusIdx = find(x, '+')
      val timesIdx = find(x, '*')
      if (timesIdx > plusIdx) {
        Some(x.substring(0, timesIdx), x.substring(timesIdx + 1))
      } else {
        None
      }
    }
  }

  object Product2 {
    def unapply(x: String): Option[(String, String)] = {
      val idx = find(x, '*')
      if (idx > 0) {
        Some(x.substring(0, idx), x.substring(idx + 1))
      } else None
    }
  }

  def find(x: String, op: Char): Int = {
    var i = x.length - 1
    var d = 0
    var idx = -1
    while (i > 0) {
      if (x.charAt(i) == '(') {
        d = d + 1
      } else if (x.charAt(i) == ')') {
        d = d - 1
      }
      if (x.charAt(i) == op && idx < 0 && d == 0) {
        idx = i
      }
      i = i - 1
    }
    idx
  }
}

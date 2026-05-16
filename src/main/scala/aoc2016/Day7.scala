package aoc2016

import aoc.NewDay

class Day7 extends NewDay(2016, 7) {
  part(1) {
    execute { in =>
      in.count(supportsTLS).toString
    }
  }

  def supportsTLS(s: String): Boolean = {
    var validOutOfBrackets = false
    var validInBrackets = true
    var inBrackets = false
    (0 to s.length - 4).foreach { i =>
      if (s(i) == '[') {
        inBrackets = true
      } else if (s(i) == ']') {
        inBrackets = false
      }
      if (s(i) == s(i + 3) && s(i) != s(i+1) && s(i+1) == s(i + 2)) {
        if (inBrackets) {
          validInBrackets = false
        } else {
          validOutOfBrackets = true
        }
      }
    }
    validInBrackets && validOutOfBrackets
  }

  part(2) {
    execute { in =>
      in.count(supportsSSL).toString
    }
  }

  def supportsSSL(s: String): Boolean = {
    findABAs(s).intersect(findBABs(s)).nonEmpty
  }

  def findABAs(s: String): Set[String] = {
    val x = Set.newBuilder[String]
    var inBrackets = false
    (0 to s.length - 3).foreach { i =>
      if (s(i) == '[') {
        inBrackets = true
      } else if (s(i) == ']') {
        inBrackets = false
      }
      if (!inBrackets && s(i) == s(i+2) && s(i) != s(i+1)) {
        x.addOne(s.substring(i, i+3))
      }
    }
    x.result()
  }

  def findBABs(s: String): Set[String] = {
    val x = Set.newBuilder[String]
    var inBrackets = false
    (0 to s.length - 3).foreach { i =>
      if (s(i) == '[') {
        inBrackets = true
      } else if (s(i) == ']') {
        inBrackets = false
      }
      if (inBrackets && s(i) == s(i+2) && s(i) != s(i+1)) {
        x.addOne(Seq(s(i+1), s(i), s(i+1)).mkString)
      }
    }
    x.result()
  }
}

object Day7Main extends Day7

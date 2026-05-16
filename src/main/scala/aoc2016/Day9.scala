package aoc2016

import aoc.NewDay

class Day9 extends NewDay(2016, 9) {
  import Day9._

  part(1) {
    execute { in =>
      in.map(decompressLength).mkString("\n")
    }
  }

  part(2) {
    execute { in =>
      in.map(decompressV2Length).mkString("\n")
    }
  }
}

object Day9 {
  def decompressLength(s: String): Long = {
    var l = 0L
    var i = 0
    while (i < s.length) {
      if (s(i) == '(') {
        val mStart = i+1
        while (s(i) != 'x') {
          i = i + 1
        }
        val len = s.substring(mStart, i).toInt
        val tStart = i+1
        while (s(i) != ')') {
          i = i + 1
        }
        val times = s.substring(tStart, i).toInt
        val d = s.substring(i+1, i + 1 + len).length
         l = l + (d * times)
        i = i + 1 + len
      } else {
        l = l + 1
        i = i + 1
      }
    }
    l
  }

  def decompressV2Length(s: String): Long = {
    var l = 0L
    var i = 0
    while (i < s.length) {
      if (s(i) == '(') {
        val mStart = i+1
        while (s(i) != 'x') {
          i = i + 1
        }
        val len = s.substring(mStart, i).toInt
        val tStart = i+1
        while (s(i) != ')') {
          i = i + 1
        }
        val times = s.substring(tStart, i).toInt
        val d = decompressV2Length(s.substring(i+1, i + 1 + len))
        l = l + (d * times)
        i = i + 1 + len
      } else {
        l = l + 1
        i = i + 1
      }
    }
    l
  }
}

object Day9Main extends Day9

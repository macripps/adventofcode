package aoc2016

import aoc.Day

class Day9 extends Day(2016, 9) {
  import Day9._

  override def part1(input: Array[String]): String = {
    input.map(decompressLength).mkString("\n")
  }

  override def part2(input: Array[String]): String = {
    input.map(decompressV2Length).mkString("\n")
  }
}

object Day9 {
  def apply() = new Day9()

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

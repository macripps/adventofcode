package aoc2015

import aoc.Day
import Day5._

class Day5 extends Day {
  override def year: Int = 2015
  override def day: Int = 5

  override def part1(input: Array[String]): String = {
    input.count(l => nicePart1(l)).toString
  }

  override def part2(input: Array[String]): String = {
    input.count(l => nicePart2(l)).toString
  }
}

object Day5 {
  def apply() = new Day5()

  def nicePart1(l: String): Boolean = {
    val vowelsNice = Seq('a', 'e', 'i', 'o', 'u').map{c => l.count(_ == c)}.sum >= 3
    val duplicatedLetters = Seq("aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz").count(p => l.contains(p)) >= 1
    val noDisallowedStrings = Seq("ab", "cd", "pq", "xy").count(c => l.contains(c)) == 0
    vowelsNice && duplicatedLetters && noDisallowedStrings
  }

  def nicePart2(l: String): Boolean = {
    val repeatedPair = {
      (0 to l.length - 4).find { i =>
        val pair = l.substring(i, i+2)
        l.substring(i + 2).contains(pair)
      } match {
        case None => false
        case Some(_) => true
      }
    }
    val separatedChar = {
      (0 to l.length - 3).find { i =>
        l(i+2) == l(i)
      } match {
        case None => false
        case Some(_) => true
      }
    }
    repeatedPair && separatedChar
  }
}

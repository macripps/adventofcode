package aoc2016

import aoc.Day

import java.security.MessageDigest

class Day5 extends Day(2016, 5) {
  override lazy val input: Array[String] = Array("ffykfhsq")

  override def part1: String = {
    val md5 = MessageDigest.getInstance("MD5")
    (1 to Int.MaxValue).to(LazyList).map { x =>
      md5.update((input(0) + x).getBytes)
      md5.digest().map("%02x" format _).mkString("")
    }.filter(_.startsWith("00000")).map { x =>
      x.charAt(5)
    }.take(8).mkString
  }

  override def part2: String = {
    val md5 = MessageDigest.getInstance("MD5")
    val password = Array.ofDim[Char](8)
    (1 to Int.MaxValue).to(LazyList).map { x =>
      md5.update((input(0) + x).getBytes)
      md5.digest().map("%02x" format _).mkString("")
    }.takeWhile(_ => password.contains(0)).filter(_.startsWith("00000")).foreach { x =>
      val pos = x.charAt(5)
      val let = x.charAt(6)
      if (pos >= '0' && pos <= '7' && password(pos - '0') == 0) {
        password(pos - '0') = let
      }
    }
    password.mkString
  }
}

object Day5 {
  def apply() = new Day5()
}

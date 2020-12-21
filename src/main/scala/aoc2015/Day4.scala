package aoc2015

import aoc.Day
import com.twitter.io.Buf

import java.security.MessageDigest

class Day4 extends Day(2015, 4) {
  override def part1: String = {
    val code = input(0)
    val md5 = MessageDigest.getInstance("MD5")
    (1 to Int.MaxValue).find { x =>
      md5.update((code + x.toString).getBytes())
      val d = md5.digest().map("%02x" format _).mkString("")
      d.startsWith("00000")
    }.map("The number is " + _).get
  }

  override def part2: String = {
    val code = input(0)
    val md5 = MessageDigest.getInstance("MD5")
    (1 to Int.MaxValue).find { x =>
      md5.update((code + x.toString).getBytes())
      val d = md5.digest().map("%02x" format _).mkString("")
      d.startsWith("000000")
    }.map("The number is " + _).get
  }
}

object Day4 {
  def apply() = new Day4()
}

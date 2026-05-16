package aoc2016

import aoc.NewDay

import java.security.MessageDigest

class Day5 extends NewDay(2016, 5) {
  val fixedInput: Array[String] = Array("ffykfhsq")

  part(1) {
    execute { _ =>
      val md5 = MessageDigest.getInstance("MD5")
      (1 to Int.MaxValue).to(LazyList).map { x =>
        md5.update((fixedInput(0) + x).getBytes)
        md5.digest().map("%02x" format _).mkString("")
      }.filter(_.startsWith("00000")).map { x =>
        x.charAt(5)
      }.take(8).mkString
    }
  }

  part(2) {
    execute { _ =>
      val md5 = MessageDigest.getInstance("MD5")
      val password = Array.ofDim[Char](8)
      (1 to Int.MaxValue).to(LazyList).map { x =>
        md5.update((fixedInput(0) + x).getBytes)
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
}

object Day5Main extends Day5

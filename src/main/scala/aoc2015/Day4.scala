package aoc2015

import aoc.NewDay

import java.security.MessageDigest

class Day4 extends NewDay(2015, 4) {
  part(1) {
    execute { in =>
      val code = in(0)
      val md5 = MessageDigest.getInstance("MD5")
      (1 to Int.MaxValue).find { x =>
        md5.update((code + x.toString).getBytes())
        val d = md5.digest().map("%02x" format _).mkString("")
        d.startsWith("00000")
      }.map("The number is " + _).get
    }
  }

  part(2) {
    execute { in =>
      val code = in(0)
      val md5 = MessageDigest.getInstance("MD5")
      (1 to Int.MaxValue).find { x =>
        md5.update((code + x.toString).getBytes())
        val d = md5.digest().map("%02x" format _).mkString("")
        d.startsWith("000000")
      }.map("The number is " + _).get
    }
  }
}

object Day4Main extends Day4

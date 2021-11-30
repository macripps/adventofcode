package aoc2016

import aoc.Day

import java.security.MessageDigest
import scala.collection.mutable

class Day14 extends Day(2016, 14) {
  override def part1(input: Array[String]): String = {
    val md = MessageDigest.getInstance("MD5")
    val stream = mutable.LinkedHashMap[Int, String]()

    def getOrUpdate(i: Int): String = {
      if (!stream.contains(i)) {
        val x = input(0) + i
        stream(i) = md.digest(x.getBytes).map("%02x" format _).mkString("")
      }
      stream(i)
    }

    findKeys(getOrUpdate).take(64).last.toString
  }

  def findKeys(getOrUpdate: Int => String): LazyList[Int] = {
    LazyList.from(0).filter { i =>
      val d = getOrUpdate(i)

      val tripled: Option[Char] = (0 to d.length-3).find { i =>
        d(i) == d(i+1) && d(i) == d(i+2)
      }.map(d.charAt)
      if (tripled.isDefined) {
        val c = tripled.get
        val z = (i + 1 to i + 1000).find { i2: Int =>
          val s2 = getOrUpdate(i2)
          if (s2.contains(f"$c$c$c$c$c")) {
            true
          } else
            false
        }
        z.isDefined
      } else {
        false
      }
    }
  }

  override def part2(input: Array[String]): String = {
    val md = MessageDigest.getInstance("MD5")
    val stream = mutable.LinkedHashMap[Int, String]()

    def getOrUpdate(i: Int): String = {
      if (!stream.contains(i)) {
        var x = input(0) + i
        (1 to 2017).foreach { _ =>
          x = md.digest(x.getBytes).map("%02x" format _).mkString("")
        }
        stream(i) = x
      }
      stream(i)
    }

    findKeys(getOrUpdate).take(64).last.toString
  }
}

object Day14 {
  def apply() = new Day14()
}

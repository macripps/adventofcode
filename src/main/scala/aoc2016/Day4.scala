package aoc2016

import aoc.Day
import Day4._

class Day4 extends Day(2016, 4) {
  override def part1(input: Array[String]): String = {
    input.map(Room).filter(_.isReal).map(_.sectorId).sum.toString
  }

  override def part2(input: Array[String]): String = {
    input.map(Room).filter(x => x.isReal && x.decodedName == "northpoleobjectstorage").head.sectorId.toString
  }
}

object Day4 {
  def apply() = new Day4()

  case class Room(s: String) {
    val sectorId: Int = {
      val t = s.split("-").last
      t.substring(0, t.indexOf('[')).toInt
    }

    val checksum: String = {
      s.substring(s.indexOf('[') + 1, s.indexOf(']'))
    }

    def decodedName: String = {
      val xLetters = s.split("-").dropRight(1).mkString("")
      xLetters.map{c =>
        val n: Char = (c + (sectorId % 26)).toChar
        if (n > 'z') {
          (n - 26).toChar
        } else {
          n
        }
      }
    }

    def isReal: Boolean = {
      val xLetters = s.split("-").dropRight(1).mkString("")
      var x = xLetters.map { x => x -> xLetters.count(_ == x) }.toMap
      val n0 = x.getOrElse(checksum(0), 0)
      x = x - checksum(0)
      val n1 = x.getOrElse(checksum(1), 0)
      x = x - checksum(1)
      val n2 = x.getOrElse(checksum(2), 0)
      x = x - checksum(2)
      val n3 = x.getOrElse(checksum(3), 0)
      x = x - checksum(3)
      val n4 = x.getOrElse(checksum(4), 0)
      x = x - checksum(4)
      if (n0 < n1 || x.exists { case (_, i) => i > n0 }) {
        false
      } else {
        if (n1 < n2 || x.exists { case (_, i) => i > n1 }) {
          false
        } else {
          if (n2 < n3 || x.exists { case (_, i) => i > n2 }) {
            false
          } else {
            if (n3 < n4 || x.exists { case (_, i) => i > n3 }) {
              false
            } else {
              !x.exists { case (_, i) => i > n4 }
            }
          }
        }
      }
    }
  }

}

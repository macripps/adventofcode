package aoc2017

import aoc.Day

import scala.collection.mutable
import scala.util.matching.Regex

class Day16 extends Day(2017, 16) {

  import Day16._

  override def part1(input: Array[String]): String = {
    var progs = Array[Char]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')
    input.head.split(",").foreach {
      case spin(amount: String) => progs = progs.takeRight(amount.toInt) ++ progs.dropRight(amount.toInt)
      case exchange(i1: String, i2: String) =>
        val t = progs(i1.toInt)
        progs(i1.toInt) = progs(i2.toInt)
        progs(i2.toInt) = t
      case partner(p1: String, p2: String) =>
        progs = progs.map { t =>
          if (p1.charAt(0) == t) {
            p2.charAt(0)
          }
          else if (p2.charAt(0) == t) {
            p1.charAt(0)
          }
          else t
        }
    }
    progs.mkString
  }

  override def part2(input: Array[String]): String = {
    var progs = Array[Char]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')
    val seen = mutable.Map[String, Int]()
    var it = 0
    while (!seen.contains(progs.mkString)) {
      seen.addOne((progs.mkString, it))
      it = it + 1
      input.head.split(",").foreach {
        case spin(amount: String) => progs = progs.takeRight(amount.toInt) ++ progs.dropRight(amount.toInt)
        case exchange(i1: String, i2: String) =>
          val t = progs(i1.toInt)
          progs(i1.toInt) = progs(i2.toInt)
          progs(i2.toInt) = t
        case partner(p1: String, p2: String) =>
          progs = progs.map { t =>
            if (p1.charAt(0) == t) {
              p2.charAt(0)
            }
            else if (p2.charAt(0) == t) {
              p1.charAt(0)
            }
            else t
          }
      }
    }
    val cycleLength = it - seen(progs.mkString)
    seen.find(x => x._2 == (1_000_000_000 % cycleLength)).get._1
  }
}

object Day16 {
  def apply() = new Day16()

  val spin: Regex = raw"s(\d+)".r
  val exchange: Regex = raw"x(\d+)/(\d+)".r
  val partner: Regex = raw"p(\w)/(\w)".r
}

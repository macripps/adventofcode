package aoc2015

import aoc.Day
import aoc2015.Day17.findSublistsThatSumsTo

class Day17 extends Day {
  override def year: Int = 2015

  override def day: Int = 17

  override def part1(input: Array[String]): String = {
    findSublistsThatSumsTo(input.toList.map(_.toInt).sortBy(-_), 150).get.length.toString
  }

  override def part2(input: Array[String]): String = {
    val results = findSublistsThatSumsTo(input.toList.map(_.toInt).sortBy(-_), 150).get
    val shortest = results.map(r => r.length).min
    results.count { r => r.length == shortest }.toString
  }
}

object Day17 {
  def apply() = new Day17()

  def findSublistsThatSumsTo(input: List[Int], total: Int): Option[List[List[Int]]] = {
    if (total == 0) {
      Some(List[List[Int]](List()))
    } else if (input.isEmpty) {
      None
    } else {
      if (total >= input.head) {
        var out: Option[List[List[Int]]] = None
        val totalMinusHead = findSublistsThatSumsTo(input.tail, total - input.head)
        if (totalMinusHead.isDefined) {
          out = Some(totalMinusHead.get.map { l => input.head +: l })
        }
        val totalWithoutHead = findSublistsThatSumsTo(input.tail, total)
        if (totalWithoutHead.isDefined) {
          if (out.isEmpty) {
            out = Some(totalWithoutHead.get)
          } else {
            out = out.map(_.appendedAll(totalWithoutHead.get))
          }
        }
        out
      } else {
        findSublistsThatSumsTo(input.tail, total)
      }
    }
  }
}

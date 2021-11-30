package aoc2016

import aoc.Day

class Day3 extends Day(2016, 3) {
  override def part1(input: Array[String]): String = {
    input.count { l =>
      val a = Seq(l.substring(2, 5).replace(" ", ""), l.substring(7, 10).replace(" ", ""), l.substring(12, 15).replace(" ", "")).map(_.toInt).sorted
      a.head + a(1) > a(2)
    }.toString
  }

  override def part2(input: Array[String]): String = {
    input.grouped(3).flatMap { l =>
      Seq(
        Seq(l(0).substring(2, 5).replace(" ", ""), l(1).substring(2, 5).replace(" ", ""), l(2).substring(2, 5).replace(" ", "")).map(_.toInt).sorted,
        Seq(l(0).substring(7, 10).replace(" ", ""), l(1).substring(7, 10).replace(" ", ""), l(2).substring(7, 10).replace(" ", "")).map(_.toInt).sorted,
        Seq(l(0).substring(12, 15).replace(" ", ""), l(1).substring(12, 15).replace(" ", ""), l(2).substring(12, 15).replace(" ", "")).map(_.toInt).sorted
      )
    }.count { a => a.head + a(1) > a(2) }.toString
  }
}

object Day3 {
  def apply() = new Day3()
}

package aoc2015

import aoc.Day

class Day2 extends Day {
  override def year: Int = 2015
  override def day: Int = 2

  override def part1(input: Array[String]): String = {
    input.map { line =>
      line.split("x").map(_.toInt).sorted match {
        case Array(l, w, h) => (2*l*w) + (2*w*h) + (2*h*l) + (l*w)
      }
    }.sum.toString
  }

  override def part2(input: Array[String]): String = {
    input.map { line =>
      line.split("x").map(_.toInt).sorted match {
        case Array(l, w, h) => (2*l) + (2*w) + (w*h*l)
      }
    }.sum.toString
  }
}

object Day2 {
  def apply() = new Day2()
}

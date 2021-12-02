package aoc2021

import aoc.Day

class Day2 extends Day(2021, 2) {
  override def part1(input: Array[String]): String = {
    var hPos = 0
    var vPos = 0
    input.foreach { l =>
      val cmd = l.split(" ")
      cmd(0) match {
        case "forward" => hPos += cmd(1).toInt
        case "down" => vPos += cmd(1).toInt
        case "up" => vPos -= cmd(1).toInt
      }
    }
    (hPos * vPos).toString
  }

  override def part2(input: Array[String]): String = {
    var hPos = 0
    var vPos = 0
    var aim = 0
    input.foreach { l =>
      val cmd = l.split(" ")
      cmd(0) match {
        case "forward" =>
          hPos += cmd(1).toInt
          vPos += aim * cmd(1).toInt
        case "down" => aim += cmd(1).toInt
        case "up" => aim -= cmd(1).toInt
      }
    }
    (hPos * vPos).toString
  }
}

object Day2 {
  def apply() = new Day2()
}

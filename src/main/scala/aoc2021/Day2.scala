package aoc2021

import aoc.Day

class Day2 extends Day(2021, 2) {
  override def part1(input: Array[String]): String = {
    val pos = input.map { l =>
      val n = l.split(' ')(1).toInt
      if (l.startsWith("forward")) (n, 0)
      else if (l.startsWith("down")) (0, n)
      else if (l.startsWith("up")) (0, -n)
      else (0,0)
    }.reduceLeft[(Int,Int)]((x: (Int, Int), y: (Int, Int)) => (x._1 + y._1, x._2 + y._2))
    (pos._1 * pos._2).toString
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

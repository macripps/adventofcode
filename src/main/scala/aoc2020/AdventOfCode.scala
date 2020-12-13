package aoc2020

import com.twitter.app.App
import com.twitter.util.Stopwatch

object AdventOfCode extends App {

  def main(): Unit = {
    println("Advent Of Code")
    println("--------------")


//    val days = Seq(Day1(), Day2(), Day3(), Day4(), Day5(), Day6(), Day7(), Day8(), Day9(), Day10(), Day11(), Day12())
    val days = Seq(Day13())

    days.foreach { day =>
      val input = readFileToIterable("aoc" + day.year + "/day" + day.day + ".input").toArray
      val x1 = Stopwatch.start()
      val result1 = day.part1(input)
      val elapsed1 = x1()
      println(day.year + "." + day.day + ".1: " + result1)
      println("---: " + elapsed1)

      val x2 = Stopwatch.start()
      val result2 = day.part2(input)
      val elapsed2 = x2()
      println(day.year + "." + day.day + ".2: " + result2)
      println("---: " + elapsed2)
    }
  }
}

package aoc2020

import aoc.Day
import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.trace.Tracer

import scala.collection.mutable

class Day15 extends Day(2020, 15) {
  override def part1(input: Array[String]): String = {
    val numbers = input.head.split(",").map(_.toInt)
    val out = Day15.iterate(numbers, 2020)
    "The 2020th number is " + out
  }

  override def part2(input: Array[String]): String = {
    val numbers = input.head.split(",").map(_.toInt)
    val out = Day15.iterate(numbers, 30000000)
    "The 30000000th number is " + out
  }

}

object Day15 {
  def apply() = new Day15()

  def iterate(nums: Array[Int], count: Int): Int = {
    val seen = mutable.LongMap[Int]()
    nums.zipWithIndex.foreach { n =>
      seen(n._1) = n._2 + 1
    }
    var lastNum = nums.last
    (nums.length until count).foreach { i =>
      val next = i - seen.getOrElse(lastNum, i)
      seen(lastNum) = i
      lastNum = next
    }
    lastNum
  }
}

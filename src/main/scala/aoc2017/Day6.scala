package aoc2017

import aoc.Day

import scala.collection.mutable

class Day6 extends Day(2017, 6) {
  override def part1(input: Array[String]): String = {
    val banks = input.head.split(raw"\s+").map(_.toInt)
    val seen = mutable.Set[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]()
    var steps = 0
    while (!seen.contains(toTuple(banks))) {
      seen.addOne(toTuple(banks))
      val blocks = banks.max
      val idx = banks.indexOf(blocks)
      banks(idx) = 0
      (1 to blocks).foreach { b =>
        banks((idx + b) % banks.length) += 1
      }
      steps = steps + 1
    }
    steps.toString
  }

  def toTuple(banks: Array[Int]): (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) = {
    (banks(0), banks(1), banks(2), banks(3), banks(4), banks(5), banks(6), banks(7), banks(8), banks(9), banks(10), banks(11), banks(12), banks(13), banks(14), banks(15))
  }

  override def part2(input: Array[String]): String = {
    val banks = input.head.split(raw"\s+").map(_.toInt)
    val seen = mutable.Map[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), Int]()
    var steps = 0
    while (!seen.keySet.contains(toTuple(banks))) {
      seen.addOne(toTuple(banks), steps)
      val blocks = banks.max
      val idx = banks.indexOf(blocks)
      banks(idx) = 0
      (1 to blocks).foreach { b =>
        banks((idx + b) % banks.length) += 1
      }
      steps = steps + 1
    }
    (steps - seen(toTuple(banks))).toString
  }
}

object Day6 {
  def apply() = new Day6()
}

package aoc2020

import aoc.Day

class Day13 extends Day(2020, 13) {
  override def part1(input: Array[String]): String = {
    val timestamp = input(0).toInt
    val busIds = input(1).split(",").filterNot(_ == "x").map(_.toInt)
    val firstBus = busIds.map(id => (id, id - timestamp % id)).minBy(f => f._2)
    "First bus with id " + firstBus._1 + " after waiting " + firstBus._2 + " minutes (product = " + (firstBus._1 * firstBus._2) + ")"
  }

  override def part2(input: Array[String]): String = {
    val busIds = input(1).split(",").zipWithIndex.filterNot(_._1 == "x").map(p => ((p._1.toInt - (p._2 % p._1.toInt)) % p._1.toInt, p._1.toInt))
    val result = aoc.chineseRemainerTheorem(busIds)
    "x = " + result._1 + " (mod " + result._2 + ")"
  }
}

object Day13 {
  def apply() = new Day13
}

package aoc2019

import aoc.Day

import scala.collection.mutable

class Day5 extends Day(2019, 5) {
  override def part1(input: Array[String]): String = {
    val program = input.head.split(',').map(_.toInt)
    val ic = new IntCode(program)
    ic.execute(mutable.Queue[Int](1))
    ""
  }

  override def part2(input: Array[String]): String = {
    ""
  }
}

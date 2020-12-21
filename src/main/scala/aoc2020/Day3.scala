package aoc2020

import aoc.Day
import aoc2020.Day3.treesHit

class Day3 extends Day(2020, 3) {

  override def part1: String = {
    "You hit " + Day3.treesHit((3, 1), input) + " trees"
  }

  override def part2: String = {
    val slopes = Array[(Int,Int)](
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2),
    )
    "You hit " + slopes.map(treesHit(_, input)).product + " trees"
  }
}

object Day3 {
  def apply() = new Day3()

  def treesHit(slope: (Int, Int), lines: Array[String]): Long = {
    var x = 0
    var y = 0
    var trees = 0L
    while (x < lines.length) {
      val pos = lines(x).charAt(y)
      if (pos == '#') {
        trees = trees + 1
      }
      y = (y + slope._1) % lines(x).length
      x = x + slope._2
    }
    trees
  }
}

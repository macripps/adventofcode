package aoc2020

import aoc.Day

class Day1 extends Day(2020, 1) {
  override def part1(input: Array[String]): String = {
    val pair = Day1.findPairThatSumTo(input.map(_.toLong).sorted, 2020)
    pair match {
      case Some(p) => "The product of " + p._1 + " and " + p._2 + " is " + (p._1 * p._2)
      case None => "No pair summed to 2020"
    }
  }

  override def part2(input: Array[String]): String = {
    val trio = Day1.findTripleThatSumTo(input.map(_.toLong).sorted, 2020)
    trio match {
      case Some(t) => "The product of " + t._1 + " and " + t._2 + " and " + t._3 + " is " + (t._1 * t._2 * t._3)
      case None => "No trio summed to 2020"
    }
  }
}

object Day1 {
  def apply(): Day1 = {
    new Day1()
  }

  def findPairThatSumTo(nums: Array[Long], total: Long): Option[(Long, Long)] = {
    var lower = 0
    var upper = nums.length - 1
    while (lower < upper && nums(lower) + nums(upper) != total) {
      if (nums(lower) + nums(upper) < total) {
        lower = lower + 1
      } else {
        upper = upper - 1
      }
    }
    if (nums(lower) + nums(upper) == total) Some(nums(lower), nums(upper)) else None
  }

  def findTripleThatSumTo(nums: Array[Long], total: Long): Option[(Long, Long, Long)] = {
    if (nums.length < 3) None else {
      var lower = 0
      var upper = nums.length - 1
      var floater = 1
      while (lower < upper && nums(lower) + nums(floater) + nums(upper) != total) {
        floater = lower + 1
        while (floater < upper && nums(lower) + nums(floater) + nums(upper) < total) {
          floater = floater + 1
        }
        if (nums(lower) + nums(floater) + nums(upper) < total) {
          lower = lower + 1
        } else {
          upper = upper - 1
        }
      }
      if (nums(lower) + nums(floater) + nums(upper) == total) {
        Some(nums(lower), nums(floater), nums(upper))
      } else {
        None
      }
    }
  }
}

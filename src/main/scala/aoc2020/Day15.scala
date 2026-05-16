package aoc2020

import aoc.NewDay

import scala.collection.mutable

class Day15 extends NewDay(2020, 15) {
  part(1) {
    execute { in =>
      val numbers = in.head.split(",").map(_.toInt)
      val out = Day15.iterate(numbers, 2020)
      "The 2020th number is " + out
    }
  }

  part(2) {
    execute { in =>
      val numbers = in.head.split(",").map(_.toInt)
      val out = Day15.iterate(numbers, 30000000)
      "The 30000000th number is " + out
    }
  }

}

object Day15Main extends Day15

object Day15 {
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

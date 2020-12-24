package aoc2020

import aoc.Day

class Day23 extends Day(2020, 23) {
  override lazy val input = "925176834".split("")

  val cups = input.map(_.toInt).toSeq
  val minCup = cups.min

  override def part1: String = {
    val array = buildArray(cups)
    var current = array(array.indexOf(cups.head))
    (1 to 100).foreach { it =>
      val item1 = array(current)
      val item2 = array(item1)
      val item3 = array(item2)
      val skip = array(item3)
      array(current) = skip

      var d = current - 1
      if (d == 0) d = array.length - 1
      while (item1 == d || item2 == d || item3 == d) {
        d = d - 1
        if (d == 0) d = array.length - 1
      }

      val n = array(d)
      array(d) = item1
      array(item3) = n

      current = skip
    }

    val result = new StringBuilder()
    var out = array(1)
    while (out != 1) {
      result.append(out)
      out = array(out)
    }

    result.toString
  }

  override def part2: String = {
    val array = buildArray(cups ++ Range.inclusive(10, 1_000_000))
    var current = array(array.indexOf(cups.head))
    (1 to 10_000_000).foreach { it =>
      val item1 = array(current)
      val item2 = array(item1)
      val item3 = array(item2)
      val skip = array(item3)
      array(current) = skip

      var d = current - 1
      if (d == 0) d = array.length - 1
      while (item1 == d || item2 == d || item3 == d) {
        d = d - 1
        if (d == 0) d = array.length - 1
      }

      val n = array(d)
      array(d) = item1
      array(item3) = n

      current = skip
    }

    val node1 = array(1)
    (node1.toLong * array(node1).toLong).toString
  }

  def buildArray(value: Seq[Int]): Array[Int] = {
    val o = Array.ofDim[Int](value.length + 1)
    var current = value.head
    (value.tail :+ current).foreach { x =>
      o(current) = x
      current = x
    }

    o
  }
}

object Day23 {
  def apply() = new Day23()
}

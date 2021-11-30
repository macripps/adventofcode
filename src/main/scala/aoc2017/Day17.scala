package aoc2017

import aoc.Day

class Day17 extends Day(2017, 17) {

  import Day17._

  override def part1(input: Array[String]): String = {
    val buffer = new Node(0)
    buffer.next = buffer
    val steps = input.head.toInt
    var n = buffer
    var o = n
    (1 to 2017).foreach { i =>
      (1 to steps).foreach { _ =>
        n = n.next
      }
      val k = new Node(i)
      k.next = n.next
      n.next = k
      o = k.next
      n = k
    }
    o.v.toString
  }

  override def part2(input: Array[String]): String = {
    val steps = input.head.toInt
    var res = 0
    var pos = 0
    (1 to 50_000_000).foreach { i =>
      pos = (pos + steps) % i
      pos = pos + 1
      if (pos == 1) {
        res = i
      }
    }
    res.toString
  }
}

object Day17 {
  def apply() = new Day17()

  class Node(val v: Int) {
    var next: Node = _
  }

}

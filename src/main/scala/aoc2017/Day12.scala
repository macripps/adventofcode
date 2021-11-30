package aoc2017

import aoc.Day

import scala.collection.mutable

class Day12 extends Day(2017, 12) {
  override def part1(input: Array[String]): String = {
    val seen = mutable.Set[Int]()
    val m = input.map { l =>
      val kv = l.split(" <-> ")
      kv(0).toInt -> kv(1).split(", ").map(_.toInt)
    }.toMap
    val q = mutable.Queue[Int](0)
    while (q.nonEmpty) {
      val k = q.dequeue()
      if (!seen.contains(k)) {
        q.addAll(m(k))
        seen.add(k)
      }
    }
    seen.size.toString
  }

  override def part2(input: Array[String]): String = {
    val m = mutable.Map.newBuilder[Int, Array[Int]]
    input.foreach { l =>
      val kv = l.split(" <-> ")
      m.addOne((kv(0).toInt, kv(1).split(", ").map(_.toInt)))
    }
    val pipes = m.result()
    var groups = 0
    while (pipes.nonEmpty) {
      val next = pipes.keys.head
      val seen = mutable.Set[Int]()
      val q = mutable.Queue[Int](next)
      while (q.nonEmpty) {
        val k = q.dequeue()
        if (!seen.contains(k)) {
          q.addAll(pipes(k))
          pipes -= k
          seen.add(k)
        }
      }
      groups = groups + 1
    }
    groups.toString
  }
}

object Day12 {
  def apply() = new Day12()
}

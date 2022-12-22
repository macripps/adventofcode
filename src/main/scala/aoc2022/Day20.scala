package aoc2022

import scala.collection.mutable

class Day20 extends aoc.Day(2022, 20) {
  // Based on https://github.com/ndrsht/adventofcode2022/blob/master/src/main/kotlin/ndrsh/puzzles/adventofcode2022/Day20.kt
  override def part1(input: Array[String]): Any = {
    val entries = mutable.Buffer[(Int, Int)]()
    entries.addAll(input.map(_.toInt).zipWithIndex)
    entries.indices.foreach { k =>
      val i = entries.indexWhere(it => it._2 == k)
      val temp = entries(i)
      entries.remove(i)
      var pos = i + temp._1
      while (pos < 0) {
        pos = pos + entries.size
      }
      entries.insert(pos % entries.size, temp)
    }
    val iZero = entries.indexWhere(it => it._1 == 0)
    Seq(1000, 2000, 3000).map { n =>
      entries((iZero + n) % entries.size)._1
    }.sum
  }

  val test =
    """1
      |2
      |-3
      |3
      |-2
      |0
      |4""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val key = 811589153
    val entries = mutable.Buffer[(Long, Int)]()
    entries.addAll(input.map(_.toLong).zipWithIndex)
    (1 to 10).foreach { _ =>
      entries.indices.foreach { k =>
        val i = entries.indexWhere(it => it._2 == k)
        val temp = entries(i)
        entries.remove(i)
        var pos = (i + (key * temp._1)) % entries.size
        if (pos < 0) {
          pos = pos + entries.size
        }
        entries.insert(pos.toInt, temp)
      }
    }
    val iZero = entries.indexWhere(it => it._1 == 0)
    Seq(1000, 2000, 3000).map { n =>
      key * entries((iZero + n) % entries.size)._1
    }.sum
  }
}

object Day20 {
  def apply() = new Day20
}
package aoc2022

import aoc.NewDay

import scala.collection.mutable

class Day20 extends NewDay(2022, 20) {
  // Based on https://github.com/ndrsht/adventofcode2022/blob/master/src/main/kotlin/ndrsh/puzzles/adventofcode2022/Day20.kt
  part(1) {
    execute { in =>
      val entries = mutable.Buffer[(Int, Int)]()
      entries.addAll(in.map(_.toInt).zipWithIndex)
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
  }

  part(2) {
    execute { in =>
      val key = 811589153
      val entries = mutable.Buffer[(Long, Int)]()
      entries.addAll(in.map(_.toLong).zipWithIndex)
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
}

object Day20Main extends Day20

package aoc2025

import aoc.{NewDay, asGroupsSeparatedByBlankLines}

import scala.collection.mutable

class Day5 extends NewDay(2025, 5) {

  part(1) {
    test(
      """3-5
        |10-14
        |16-20
        |12-18
        |
        |1
        |5
        |8
        |11
        |17
        |32""".stripMargin -> 3)
    execute { ls =>
      val separated = asGroupsSeparatedByBlankLines(ls)
      val ranges = separated.head
      val ids = separated.tail.head
      val idRs = ranges.map { s =>
        val Array(l, h) = s.split('-').map(_.toLong)
        (l, h)
      }
      ids.map(_.toLong).count { id =>
        idRs.exists { r => r._1 <= id && r._2 >= id }
      }
    }
  }

  part(2) {
    test(
      """3-5
        |10-14
        |16-20
        |12-18
        |
        |1
        |5
        |8
        |11
        |17
        |32""".stripMargin -> 14)
    test(
      """1-1
        |2-2
        |3-3
        |4-4
        |5-5
        |
        |1""".stripMargin -> 5)
    test(
      """1-100
        |1-1
        |1-2
        |1-3
        |1-4
        |1-5
        |1-6
        |1-7
        |1-8
        |1-9
        |1-10""".stripMargin -> 100)
    test(
      """200-300
        |100-101
        |1-1
        |2-2
        |3-3
        |1-3
        |1-3
        |2-2
        |50-70
        |10-10
        |98-99
        |99-99
        |99-99
        |99-100
        |1-1
        |2-1
        |100-100
        |100-100
        |100-101
        |200-300
        |201-300
        |202-300
        |250-251
        |98-99
        |100-100
        |100-101
        |1-101
        |
        |202""".stripMargin -> 202)
    test(
      """10-20
        |14-28
        |
        |19""".stripMargin -> 19)
    execute { ls =>
      val separated = asGroupsSeparatedByBlankLines(ls)
      val rangeStrings = separated.head.map { s => s.split('-').map(_.toLong) }.toList.sortBy(r => r.head)
      val ranges = mutable.Stack[(Long, Long, Long)]((rangeStrings.head(0), rangeStrings.head(1), rangeStrings.head(1) + 1L - rangeStrings.head(0)))
      rangeStrings.tail.foreach { s =>
        val Array(l, h) = s
        var newL = l
        var newH = h
        val range = ranges.head
        if (newL <= range._2 + 1) {
          ranges.pop()
          newL = Math.min(newL, range._1)
          newH = Math.max(newH, range._2)
        }
        ranges.push((newL, newH, newH - newL + 1))
      }
      ranges.toList.map(_._3).sum
    }
  }
}

object Day5Main extends Day5
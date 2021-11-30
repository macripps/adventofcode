package aoc2016

import aoc.Day

import scala.collection.immutable.NumericRange

class Day20 extends Day(2016, 20) {
  override def part1(input: Array[String]): String = {
    val ranges = input.map(x => {
      val a = x.split("-")
      Range.Long.inclusive(a(0).toLong, a(1).toLong, 1L)
    }).sortBy(x => x.start)
    var k = ranges.head
    ranges.drop(1).foreach { r =>
      k = extend(k, r)
    }
    (1L + k.end).toString
  }

  def extend(r1: NumericRange.Inclusive[Long], r2: NumericRange.Inclusive[Long]): NumericRange.Inclusive[Long] = {
    if (r1.contains(r2.start) || r2.contains(r1.end)) {
      Range.Long.inclusive(math.min(r1.start, r2.start), math.max(r1.end, r2.end), 1L)
    } else if (r2.start.asInstanceOf[Long] == 1L + r1.end) {
      Range.Long.inclusive(r1.start, r2.end, 1L)
    } else {
      r1
    }
  }

  override def part2(input: Array[String]): String = {
    val mergedRanges = List.newBuilder[NumericRange.Inclusive[Long]]
    val ranges = input.map(x => {
      val a = x.split("-")
      Range.Long.inclusive(a(0).toLong, a(1).toLong, 1L)
    }).sortBy(x => x.start)
    var i = 1
    var r = ranges(0)
    while (i < ranges.length) {
      val k = ranges(i)
      if (k.start.asInstanceOf[Long] <= 1L + r.end) {
        r = Range.Long.inclusive(r.start, math.max(r.end, k.end), 1L)
      } else {
        mergedRanges.addOne(r)
        r = k
      }
      i = i + 1
    }
    mergedRanges.addOne(r)
    val merged = mergedRanges.result()
    var allowed = 0L
    merged.indices.drop(1).foreach { i =>
      allowed += (merged(i).start.asInstanceOf[Long] - 1L - merged(i - 1).end.asInstanceOf[Long])
    }

    allowed.toString
  }
}

object Day20 {
  def apply() = new Day20()
}

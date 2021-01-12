package aoc2017

import aoc.Day

class Day10 extends Day(2017, 10) {
  override def part1: String = {
    val r = (0 to 255).toArray
    var pos = 0
    var skip = 0
    input.head.split(",").map(_.toInt).foreach { i =>
      val d = pos + i - 1
      (pos to (pos + i / 2)).zip(Range.inclusive(d, pos + i / 2, -1)).foreach { ps =>
        val t = r(ps._1 % r.length)
        r(ps._1 % r.length) = r(ps._2 % r.length)
        r(ps._2 % r.length) = t
      }
      pos = (pos + i + skip) % r.length
      skip = skip + 1
    }
    (r(0) * r(1)).toString
  }

  override def part2: String = {
    val lengths = input.head.toCharArray.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)
    val r = (0 to 255).toArray
    var pos = 0
    var skip = 0
    (1 to 64).foreach { _ =>
      lengths.foreach { i =>
        val d = pos + i - 1
        (pos to (pos + i / 2)).zip(Range.inclusive(d, pos + i / 2, -1)).foreach { ps =>
          val t = r(ps._1 % r.length)
          r(ps._1 % r.length) = r(ps._2 % r.length)
          r(ps._2 % r.length) = t
        }
        pos = (pos + i + skip) % r.length
        skip = skip + 1
      }
    }
    r.grouped(16).map{ h =>
      h.fold(0) { case k => k._1 ^ k._2 }
    }.map(n => "%02x" format n).mkString
  }
}

object Day10 {
  def apply() = new Day10()
}

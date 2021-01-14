package aoc2017

import aoc.Day

import scala.collection.mutable

class Day25 extends Day(2017, 25) {
  override def part1: String = {
    val tape = mutable.Map[Int, Boolean]()
    var state = 1
    var c = 0
    (1 to 12302209).foreach { _ =>
      val v = tape.getOrElse(c, false)
      (state, v) match {
        case (1, false) =>
          tape(c) = true
          c = c + 1
          state = 2
        case (1, true) =>
          tape(c) = false
          c = c - 1
          state = 4
        case (2, false) =>
          tape(c) = true
          c = c + 1
          state = 3
        case (2, true) =>
          tape(c) = false
          c = c + 1
          state = 6
        case (3, false) =>
          tape(c) = true
          c = c - 1
        case (3, true) =>
          c = c - 1
          state = 1
        case (4, false) =>
          c = c - 1
          state = 5
        case (4, true) =>
          c = c + 1
          state = 1
        case (5, false) =>
          tape(c) = true
          c = c - 1
          state = 1
        case (5, true) =>
          tape(c) = false
          c = c + 1
          state = 2
        case (6, false) =>
          c = c + 1
          state = 3
        case (6, true) =>
          tape(c) = false
          c = c + 1
          state = 5
      }
    }
    tape.count(x => x._2).toString
  }

  override def part2: String = ""
}

object Day25 {
  def apply() = new Day25()
}

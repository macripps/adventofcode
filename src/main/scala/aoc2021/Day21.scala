package aoc2021

import aoc.Day
import aoc2021.Day21.State

import scala.collection.mutable

class Day21 extends Day(2021, 21) {
  override def part1(input: Array[String]): String = {
    var p1 = 10
    var s1 = 0
    var p2 = 6
    var s2 = 0
    var rolls = 0
    var n = 1
    var total = 0
    while (s1 < 1000 && s2 < 1000) {
      p1 = p1 + n
      while (p1 > 10) p1 = p1 - 10
      n = n + 1
      if (n > 100) n = n - 100
      p1 = p1 + n
      while (p1 > 10) p1 = p1 - 10
      n = n + 1
      if (n > 100) n = 1
      p1 = p1 + n
      while (p1 > 10) p1 = p1 - 10
      n = n + 1
      if (n > 100) n = 1
      rolls = rolls + 3
      s1 = s1 + p1
      if (s1 >= 1000) {
        total = s2 * rolls
      } else {
        p2 = p2 + n
        while (p2 > 10) p2 = p2 - 10
        n = n + 1
        if (n > 100) n = n - 100
        p2 = p2 + n
        while (p2 > 10) p2 = p2 - 10
        n = n + 1
        if (n > 100) n = 1
        p2 = p2 + n
        while (p2 > 10) p2 = p2 - 10
        n = n + 1
        if (n > 100) n = 1
        rolls = rolls + 3
        s2 = s2 + p2
        if (s2 >= 1000) {
          total = s1 * rolls
        }
      }
    }
    total.toString
  }

  override def part2(input: Array[String]): String = {
    val rolls = Map[Int, Long](3 -> 1L, 4 -> 3L, 5 -> 6L, 6 -> 7L, 7 -> 6L, 8 -> 3L, 9 -> 1L)
    val state = State(10, 0, 6, 0)
    var win1: Long = 0
    var win2: Long = 0

    var counts = Map[State, Long](state -> 1L)
    while (counts.nonEmpty) {
      println(counts)
      var next = Map[State, Long]().withDefaultValue(0L)
      rolls.foreach { case (rollSum, rollCount) =>
        counts.foreach { case (state, count) =>
          val p1Spot = (state.p1Spot + rollSum - 1) % 10 + 1
          val p1Points = state.p1Points + p1Spot
          if (p1Points >= 21) {
            win1 = win1 + count * rollCount
          } else {
            val s = State(p1Spot, p1Points, state.p2Spot, state.p2Points)
            next += (s -> (next(s) + (count * rollCount)))
          }
        }
      }
      counts = next
      next = Map[State, Long]().withDefaultValue(0L)
      rolls.foreach { case (rollSum, rollCount) =>
        counts.foreach { case (state, count) =>
          val p2Spot = (state.p2Spot + rollSum - 1) % 10 + 1
          val p2Points = state.p2Points + p2Spot
          if (p2Points >= 21) {
            win2 = win2 + count * rollCount
          } else {
            val s = State(state.p1Spot, state.p1Points, p2Spot, p2Points)
            next += (s -> (next(s) + (count * rollCount)))
          }
        }
      }
      counts = next
    }

    math.max(win1, win2).toString
  }
}

object Day21 {
  def apply() = new Day21

  case class State(p1Spot: Int, p1Points: Int, p2Spot: Int, p2Points: Int)
}

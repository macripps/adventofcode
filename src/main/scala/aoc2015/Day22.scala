package aoc2015

import aoc.Day
import Day22._

class Day22 extends Day(2015, 22) {
  val boss: Boss = Boss(input(0).drop(12).toInt, input(1).drop(8).toInt)

  override def part1: String = {
    ""
  }

  override def part2: String = ""
}

object Day22 {
  def apply() = new Day22()

  case class Player(hitPoints: Int)
  case class Boss(hitPoints: Int, damage: Int)
  case class State(p: Player, b: Boss, effects: List[Unit], manaSpent: Int)
}

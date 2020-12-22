package aoc2015

import aoc.Day
import aoc2015.Day21._

import scala.annotation.tailrec

class Day21 extends Day(2015, 21) {

  val weapons = Set(
    Weapon(8, 4),
    Weapon(10, 5),
    Weapon(25, 6),
    Weapon(40, 7),
    Weapon(74, 8),
  )

  val armors: Set[Option[Armor]] = Set(
    Some(Armor(13, 1)),
    Some(Armor(31, 2)),
    Some(Armor(53, 3)),
    Some(Armor(75, 4)),
    Some(Armor(102, 5)),
    None)

  val rings: Set[Option[Ring]] = Set(
    Some(Ring(25, 1, 0)),
    Some(Ring(50, 2, 0)),
    Some(Ring(100, 3, 0)),
    Some(Ring(20, 0, 1)),
    Some(Ring(40, 0, 2)),
    Some(Ring(80, 0, 3)),
    None
  )

  val allLoadouts: Set[Loadout] = {
    weapons.flatMap { w =>
      armors.flatMap { a =>
        rings.flatMap { ring1 =>
          val rings2 = if (ring1.isEmpty) rings else (rings - ring1)
          rings2.map { ring2 =>
            Loadout(w, a, ring1, ring2)
          }
        }
      }
    }
  }

  val boss: Boss = Boss(input(0).drop(12).toInt, input(1).drop(8).toInt, input(2).drop(7).toInt)

  override def part1: String = {
    val cheapestLoadout = allLoadouts.filter { l =>
      combat(Player(100, l.damage, l.armor), boss, playerTurn = true) == Win
    }.minBy { _.cost }
    println(cheapestLoadout)
    cheapestLoadout.cost.toString
  }

  override def part2: String = {
    val mostExpensiveLoadout = allLoadouts.filter { l =>
      combat(Player(100, l.damage, l.armor), boss, playerTurn = true) == Loss
    }.maxBy { _.cost }
    println(mostExpensiveLoadout)
    mostExpensiveLoadout.cost.toString
  }

}

object Day21 {
  def apply() = new Day21()

  case class Loadout(w: Weapon, a: Option[Armor], r1: Option[Ring], r2: Option[Ring]) {
    val cost: Int =
      w.cost + a.map(_.cost).getOrElse(0) + r1.map(_.cost).getOrElse(0) + r2.map(_.cost).getOrElse(0)

    val damage: Int =
      w.damage + r1.map(_.damage).getOrElse(0) + r2.map(_.damage).getOrElse(0)

    val armor: Int =
      a.map(_.armor).getOrElse(0) + r1.map(_.armor).getOrElse(0) + r2.map(_.armor).getOrElse(0)
  }

  case class Player(hitPoints: Int, damage: Int, armor: Int)
  case class Weapon(cost: Int, damage: Int)
  case class Armor(cost: Int, armor: Int)
  case class Ring(cost: Int, damage: Int, armor: Int)

  case class Boss(hitPoints: Int, damage: Int, armor: Int)

  trait Result
  case object Win extends Result
  case object Loss extends Result

  @tailrec
  def combat(player: Player, boss: Boss, playerTurn: Boolean): Result = {
    if (playerTurn) {
      if (player.hitPoints <= 0) {
        Loss
      } else {
        combat(player, boss.copy(hitPoints = boss.hitPoints - math.max(1, player.damage - boss.armor)), playerTurn = false)
      }
    } else {
      if (boss.hitPoints <= 0) {
        Win
      } else {
        combat(player.copy(hitPoints = player.hitPoints - math.max(1, boss.damage - player.armor)), boss, playerTurn = true)
      }
    }
  }
}

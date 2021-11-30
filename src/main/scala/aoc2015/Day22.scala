package aoc2015

import aoc.Day
import Day22._

import scala.collection.mutable

class Day22 extends Day(2015, 22) {
  override def part1(input: Array[String]): String = {
    val p1 = Player(mana = 500, armor = 0, hitPoints = 50)
    val boss = Boss(71, 10)

    val searchSpace = mutable.Buffer[mutable.Buffer[Spell]]()
    searchSpace.addAll(spells.map(mutable.Buffer(_)))

    var minCost = Int.MaxValue

    while (searchSpace.nonEmpty) {
      val spellsToApply = searchSpace.head
      if (debug) {
        println("Checking " + spellsToApply.length + " (" + spellsToApply.map(_.manaCost).sum + " mana)")
      }
      searchSpace.remove(0)

      var state = State(p = p1, b = boss, manaSpent = 0, effects = Seq(), spells = Seq())

      while (spellsToApply.nonEmpty && state.p.hitPoints > 0 && state.b.hitPoints > 0 && state.manaSpent < minCost) {
        if (debug) {
          println("-- Player turn --")
          println("- Player has " + state.p.hitPoints + " hit points, " + state.p.armor + " armor, " + state.p.mana + " mana")
          println("- Boss has " + state.b.hitPoints + " hit points")
        }

        state.effects.foreach { e =>
          state = e.applyTo(state)
        }

        val spell = spellsToApply.head
        spellsToApply.remove(0)
        if (debug) {
          println("Player casts " + spell + ".")
        }
        state = spell.applyTo(state)

        if (debug) {
          println()
          println("--Boss turn --")
          println("- Player has " + state.p.hitPoints + " hit points, " + state.p.armor + " armor, " + state.p.mana + " mana")
          println("- Boss has " + state.b.hitPoints + " hit points")
        }
        state.effects.foreach { e =>
          state = e.applyTo(state)
        }

        if (state.b.hitPoints > 0) {
          val damage = math.max(1, state.b.damage - state.p.armor)
          if (debug) {
            println("Boss attacks for " + damage + " damage.")
          }
          state = state.copy(p = state.p.copy(hitPoints = state.p.hitPoints - damage))
        }

        if (debug) {
          println()
        }
      }

      if (state.b.hitPoints <= 0) {
        val sum = state.spells.map(_.manaCost).sum
        println("Player wins - and cast " + state.spells + " for a total of " + sum + " mana.")
        if (sum < minCost) {
          minCost = sum
        }
      } else if (state.p.hitPoints <= 0) {
        //        println("Player loses")
      } else if (state.manaSpent < minCost) {
        //        println("Not enough spells")

        // Indeterminate, find more spells to cast
        val options = spells.filter { s =>
          val canAfford = s.manaCost <= state.p.mana
          s match {
            case MagicMissile => canAfford
            case Drain => canAfford
            case Poison => if (!state.effects.exists(x => x.isInstanceOf[PoisonEffect] && x.asInstanceOf[PoisonEffect].turnsLeft>1)) {
              canAfford
            } else false
            case Shield => if (!state.effects.exists(x => x.isInstanceOf[ShieldEffect] && x.asInstanceOf[ShieldEffect].turnsLeft>1)) {
              canAfford
            } else false
            case Recharge => if (!state.effects.exists(x => x.isInstanceOf[RechargeEffect] && x.asInstanceOf[RechargeEffect].turnsLeft>1)) {
              canAfford
            } else false
          }
        }

        if (options.nonEmpty) {
          val nextChoices = options.map { s => state.spells.toBuffer :+ s }
          if (debug) {
            println("Adding: " + nextChoices)
          }
          searchSpace.prependAll(nextChoices)
        }
      }
    }

    minCost.toString
  }

  override def part2(input: Array[String]): String = {
    val p1 = Player(mana = 500, armor = 0, hitPoints = 50)
    val boss = Boss(71, 10)

    val searchSpace = mutable.Buffer[mutable.Buffer[Spell]]()
    searchSpace.addAll(spells.map(mutable.Buffer(_)))

    var minCost = Int.MaxValue

    while (searchSpace.nonEmpty) {
      val spellsToApply = searchSpace.head
      if (debug) {
        println("Checking " + spellsToApply.length + " (" + spellsToApply.map(_.manaCost).sum + " mana)")
      }
      searchSpace.remove(0)

      var state = State(p = p1, b = boss, manaSpent = 0, effects = Seq(), spells = Seq())

      while (spellsToApply.nonEmpty && state.p.hitPoints > 0 && state.b.hitPoints > 0 && state.manaSpent < minCost) {
        if (debug) {
          println("-- Player turn --")
          println("- Player has " + state.p.hitPoints + " hit points, " + state.p.armor + " armor, " + state.p.mana + " mana")
          println("- Boss has " + state.b.hitPoints + " hit points")
        }

        state = state.copy(p = state.p.copy(hitPoints = state.p.hitPoints - 1))

        state.effects.foreach { e =>
          state = e.applyTo(state)
        }

        val spell = spellsToApply.head
        spellsToApply.remove(0)
        if (debug) {
          println("Player casts " + spell + ".")
        }
        state = spell.applyTo(state)

        if (debug) {
          println()
          println("--Boss turn --")
          println("- Player has " + state.p.hitPoints + " hit points, " + state.p.armor + " armor, " + state.p.mana + " mana")
          println("- Boss has " + state.b.hitPoints + " hit points")
        }
        state.effects.foreach { e =>
          state = e.applyTo(state)
        }

        if (state.b.hitPoints > 0) {
          val damage = math.max(1, state.b.damage - state.p.armor)
          if (debug) {
            println("Boss attacks for " + damage + " damage.")
          }
          state = state.copy(p = state.p.copy(hitPoints = state.p.hitPoints - damage))
        }

        if (debug) {
          println()
        }
      }

      if (state.b.hitPoints <= 0) {
        val sum = state.spells.map(_.manaCost).sum
        println("Player wins - and cast " + state.spells + " for a total of " + sum + " mana.")
        if (sum < minCost) {
          minCost = sum
        }
      } else if (state.p.hitPoints <= 0) {
        //        println("Player loses")
      } else if (state.manaSpent < minCost) {
        //        println("Not enough spells")

        // Indeterminate, find more spells to cast
        val options = spells.filter { s =>
          val canAfford = s.manaCost <= state.p.mana
          s match {
            case MagicMissile => canAfford
            case Drain => canAfford
            case Poison => if (!state.effects.exists(x => x.isInstanceOf[PoisonEffect] && x.asInstanceOf[PoisonEffect].turnsLeft>1)) {
              canAfford
            } else false
            case Shield => if (!state.effects.exists(x => x.isInstanceOf[ShieldEffect] && x.asInstanceOf[ShieldEffect].turnsLeft>1)) {
              canAfford
            } else false
            case Recharge => if (!state.effects.exists(x => x.isInstanceOf[RechargeEffect] && x.asInstanceOf[RechargeEffect].turnsLeft>1)) {
              canAfford
            } else false
          }
        }

        if (options.nonEmpty) {
          val nextChoices = options.map { s => state.spells.toBuffer :+ s }
          if (debug) {
            println("Adding: " + nextChoices)
          }
          searchSpace.prependAll(nextChoices)
        }
      }

    }

    minCost.toString
  }
}

object Day22 {
  def apply() = new Day22()

  val debug = false

  trait Result

  case object Unknown extends Result

  case object PlayerWins extends Result

  case object BossWins extends Result

  case class Player(mana: Int, armor: Int, hitPoints: Int)

  case class Boss(hitPoints: Int, damage: Int)

  case class State(p: Player, b: Boss, effects: Seq[Effect], manaSpent: Int, spells: Seq[Spell])

  trait Effect {
    def applyTo(state: State): State
  }

  case class ShieldEffect(turnsLeft: Int = 6) extends Effect {
    override def applyTo(state: State): State = {
      val effects = state.effects.filter(f => !f.isInstanceOf[ShieldEffect])
      if (debug) {
        println("Shield's timer is now " + (turnsLeft - 1) + ".")
      }
      if (turnsLeft > 1) {
        val p = state.p.copy(armor = 7)
        state.copy(p = p, effects = effects :+ ShieldEffect(turnsLeft - 1))
      } else {
        val p = state.p.copy(armor = 0)
        if (debug) {
          println("Shield wears off, decreasing armor by 7.")
        }
        state.copy(p = p, effects = effects)
      }
    }
  }

  case class PoisonEffect(turnsLeft: Int = 6) extends Effect {
    override def applyTo(state: State): State = {
      val b = state.b.copy(hitPoints = state.b.hitPoints - 3)
      val effects = state.effects.filter(f => !f.isInstanceOf[PoisonEffect])
      if (debug) {
        println("Poison deals 3 damage; its timer is now " + (turnsLeft - 1) + ".")
      }
      if (turnsLeft > 1) {
        state.copy(b = b, effects = effects :+ PoisonEffect(turnsLeft - 1))
      } else {
        if (debug) {
          println("Poison wears off.")
        }
        state.copy(b = b, effects = effects)
      }
    }
  }

  case class RechargeEffect(turnsLeft: Int = 5) extends Effect {
    override def applyTo(state: State): State = {
      val effects = state.effects.filter(f => !f.isInstanceOf[RechargeEffect])
      if (debug) {
        println("Recharge provides 101 mana; its is now " + (turnsLeft - 1) + ".")
      }
      val p = state.p.copy(mana = state.p.mana + 101)
      if (turnsLeft > 1) {
        state.copy(p = p, effects = effects :+ RechargeEffect(turnsLeft - 1))
      } else {
        if (debug) {
          println("Recharge wears off.")
        }
        state.copy(p = p, effects = effects)
      }
    }
  }

  trait Spell {
    val manaCost: Int

    def applyTo(state: State): State
  }

  case object MagicMissile extends Spell {
    override val manaCost: Int = 53

    override def applyTo(state: State): State = {
      val p = state.p.copy(mana = state.p.mana - manaCost)
      val b = state.b.copy(hitPoints = state.b.hitPoints - 4)
      state.copy(p = p, b = b, effects = state.effects, manaSpent = state.manaSpent + manaCost, spells = state.spells :+ this)
    }
  }

  case object Drain extends Spell {
    override val manaCost: Int = 73

    override def applyTo(state: State): State = {
      val p = state.p.copy(mana = state.p.mana - manaCost, hitPoints = state.p.hitPoints + 2)
      val b = state.b.copy(hitPoints = state.b.hitPoints - 2)
      state.copy(p = p, b = b, effects = state.effects, manaSpent = state.manaSpent + manaCost, spells = state.spells :+ this)
    }
  }

  case object Shield extends Spell {
    override val manaCost: Int = 113

    override def applyTo(state: State): State = {
      val p = state.p.copy(mana = state.p.mana - manaCost)
      state.copy(p = p, b = state.b, effects = state.effects :+ ShieldEffect(), manaSpent = state.manaSpent + manaCost, spells = state.spells :+ this)
    }
  }

  case object Poison extends Spell {
    override val manaCost: Int = 173

    override def applyTo(state: State): State = {
      val p = state.p.copy(mana = state.p.mana - manaCost)
      state.copy(p = p, b = state.b, effects = state.effects :+ PoisonEffect(), manaSpent = state.manaSpent + manaCost, spells = state.spells :+ this)
    }
  }

  case object Recharge extends Spell {
    override val manaCost: Int = 229

    override def applyTo(state: State): State = {
      val p = state.p.copy(mana = state.p.mana - manaCost)
      state.copy(p = p, b = state.b, effects = state.effects :+ RechargeEffect(), manaSpent = state.manaSpent + manaCost, spells = state.spells :+ this)
    }
  }

  val spells: Seq[Spell] = Seq(MagicMissile, Drain, Shield, Poison, Recharge)

}

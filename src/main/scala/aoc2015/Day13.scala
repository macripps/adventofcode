package aoc2015

import aoc.Day
import Day13._

import scala.collection.mutable
import scala.util.matching.Regex

class Day13 extends Day(2015, 13) {
  override def part1(input: Array[String]): String = {
    val happiness = toHappinessMap(input)

    val availablePeople = happiness.keys.toList
    val happiestOrder = availablePeople.permutations.maxBy { p: List[String] =>
      happinessCalc(happiness, p) + happinessCalc(happiness, p.reverse)
    }
    happiestOrder.mkString(",") + ": " + (happinessCalc(happiness, happiestOrder) + happinessCalc(happiness, happiestOrder.reverse))
  }

  private def toHappinessMap(input: Array[String]) = {
    val happiness = mutable.Map[String, mutable.Map[String, Int]]()
    input.foreach {
      case gain(name1, change, name2) =>
        if (happiness.contains(name1)) {
          happiness(name1) += (name2 -> change.toInt)
        } else {
          happiness(name1) = mutable.Map((name2, change.toInt))
        }
      case loss(name1, change, name2) =>
        if (happiness.contains(name1)) {
          happiness(name1) += (name2 -> -change.toInt)
        } else {
          happiness(name1) = mutable.Map((name2, -change.toInt))
        }
    }
    happiness
  }

  override def part2(input: Array[String]): String = {
    val happiness = toHappinessMap(input)
    happiness("me") = mutable.Map[String, Int]()

    val availablePeople = happiness.keys.toList
    availablePeople.foreach { p =>
      happiness("me") += (p -> 0)
      happiness(p) += ("me" -> 0)
    }

    val happiestOrder = availablePeople.permutations.maxBy { p: List[String] =>
      happinessCalc(happiness, p) + happinessCalc(happiness, p.reverse)
    }
    happiestOrder.mkString(",") + ": " + (happinessCalc(happiness, happiestOrder) + happinessCalc(happiness, happiestOrder.reverse))
  }
}

object Day13 {
  def apply() = new Day13()

  val gain: Regex = raw"(\w+) would gain (\d+) happiness units by sitting next to (\w+).".r
  val loss: Regex = raw"(\w+) would lose (\d+) happiness units by sitting next to (\w+).".r

  def happinessCalc(happiness: collection.Map[String, collection.Map[String, Int]], perm: List[String]): Int = {
    var distance = 0
    var person = perm.head
    var toVisit = perm.tail :+ person
    while (toVisit.nonEmpty) {
      val next = toVisit.head
      distance += happiness(person)(next)
      person = next
      toVisit = toVisit.tail
    }
    distance
  }
}

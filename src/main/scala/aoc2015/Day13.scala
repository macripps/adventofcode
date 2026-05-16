package aoc2015

import aoc.NewDay
import Day13._

import scala.collection.mutable
import scala.util.matching.Regex

class Day13 extends NewDay(2015, 13) {
  part(1) {
    execute { in =>
      val happiness = toHappinessMap(in)

      val availablePeople = happiness.keys.toList
      val happiestOrder = availablePeople.permutations.maxBy { p: List[String] =>
        happinessCalc(happiness, p) + happinessCalc(happiness, p.reverse)
      }
      happiestOrder.mkString(",") + ": " + (happinessCalc(happiness, happiestOrder) + happinessCalc(happiness, happiestOrder.reverse))
    }
  }

  part(2) {
    execute { in =>
      val happiness = toHappinessMap(in)
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

  private def toHappinessMap(in: Array[String]) = {
    val happiness = mutable.Map[String, mutable.Map[String, Int]]()
    in.foreach {
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
}

object Day13 {
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

object Day13Main extends Day13

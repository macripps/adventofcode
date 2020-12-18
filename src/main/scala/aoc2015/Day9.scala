package aoc2015

import aoc.Day
import Day9._

import scala.collection.mutable

class Day9 extends Day {
  override def year: Int = 2015
  override def day: Int = 9

  override def part1(input: Array[String]): String = {
    val towns = mutable.Map[String, mutable.Map[String, Int]]()
    input.foreach {
      case Line(town1, town2, distance) => {
        if (towns.contains(town1)) {
          towns(town1) += (town2 -> distance)
        } else {
          towns(town1) = mutable.Map((town2, distance))
        }
        if (towns.contains(town2)) {
          towns(town2) += (town1 -> distance)
        } else {
          towns(town2) = mutable.Map((town1, distance))
        }
      }
    }
    val availableTowns = towns.keys.toList
    val shortestRoute = availableTowns.permutations.minBy { p: List[String] =>
      routeDistance(towns, p)
    }

    shortestRoute.mkString(",") + ": " + routeDistance(towns, shortestRoute)
  }

  override def part2(input: Array[String]): String = {
    val towns = mutable.Map[String, mutable.Map[String, Int]]()
    input.foreach {
      case Line(town1, town2, distance) => {
        if (towns.contains(town1)) {
          towns(town1) += (town2 -> distance)
        } else {
          towns(town1) = mutable.Map((town2, distance))
        }
        if (towns.contains(town2)) {
          towns(town2) += (town1 -> distance)
        } else {
          towns(town2) = mutable.Map((town1, distance))
        }
      }
    }
    val availableTowns = towns.keys.toList
    val longestRoute = availableTowns.permutations.maxBy { p: List[String] =>
      routeDistance(towns, p)
    }

    longestRoute.mkString(",") + ": " + routeDistance(towns, longestRoute)
  }
}

object Day9 {
  def apply() = new Day9()

  object Line {
    def unapply(line: String): Option[(String, String, Int)] = {
      val tds = line.split(" = ")
      val ts = tds(0).split(" to ")
      Some(ts(0), ts(1), tds(1).toInt)
    }
  }

  def routeDistance(towns: mutable.Map[String, mutable.Map[String, Int]], route: List[String]): Int = {
    var distance = 0
    var loc = route.head
    var toVisit = route.tail
    while (toVisit.nonEmpty) {
      val next = toVisit.head
      distance += towns(loc)(next)
      loc = next
      toVisit = toVisit.tail
    }
    distance
  }
}

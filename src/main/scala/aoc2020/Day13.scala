package aoc2020

import aoc.NewDay

class Day13 extends NewDay(2020, 13) {
  part(1) {
    execute { in =>
      val timestamp = in(0).toInt
      val busIds = in(1).split(",").filterNot(_ == "x").map(_.toInt)
      val firstBus = busIds.map(id => (id, id - timestamp % id)).minBy(f => f._2)
      "First bus with id " + firstBus._1 + " after waiting " + firstBus._2 + " minutes (product = " + (firstBus._1 * firstBus._2) + ")"
    }
  }

  part(2) {
    execute { in =>
      val busIds = in(1).split(",").zipWithIndex.filterNot(_._1 == "x").map(p => ((p._1.toInt - (p._2 % p._1.toInt)) % p._1.toInt, p._1.toInt))
      val result = aoc.chineseRemainerTheorem(busIds)
      "x = " + result._1 + " (mod " + result._2 + ")"
    }
  }
}

object Day13Main extends Day13

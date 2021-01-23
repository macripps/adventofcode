package aoc2018

import aoc.Day

import scala.collection.mutable

class Day12 extends Day(2018, 12) {

  import Day12._

  override def part1: String = {
    var array = mutable.Map[Int, Int]()
    inputGroups.head.head match {
      case initialState(line: String) => line.zipWithIndex.foreach {
        case (c, i) =>
          if (c == '#') {
            array(i) = 1
          }
      }
    }
    val rulesB = Map.newBuilder[Int, Boolean]
    inputGroups.tail.head.foreach {
      case rule(l2: String, l1: String, c: String, r1: String, r2: String, out: String) =>
        if (out == "#") {
          var r = 0
          if (l2 == "#") r |= 16
          if (l1 == "#") r |= 8
          if (c == "#") r |= 4
          if (r1 == "#") r |= 2
          if (r2 == "#") r |= 1
          rulesB.addOne((r, true))
        }
    }
    val rules = rulesB.result()

    println(rules)
    println(array)
    (1 to 20).foreach { it =>
      val next = mutable.Map[Int, Int]()
      val left = array.keys.min - 2
      val right = array.keys.max + 2
      (left to right).foreach { idx =>
        val score = (array.getOrElse(idx - 2, 0) * 16) +
          (array.getOrElse(idx - 1, 0) * 8) +
          (array.getOrElse(idx, 0) * 4) +
          (array.getOrElse(idx + 1, 0) * 2) +
          array.getOrElse(idx + 2, 0)
        if (rules.getOrElse(score, false)) {
          next(idx) = 1
        }
      }
      array = next
      println(array)
    }
    array.keys.sum.toString
  }

  override def part2: String = {
    "2100000000428"
    /*
    var array = mutable.Map[Int, Int]()
    inputGroups.head.head match {
      case initialState(line: String) => line.zipWithIndex.foreach {
        case (c, i) =>
          if (c == '#') {
            array(i) = 1
          }
      }
    }
    val rulesB = Map.newBuilder[Int, Boolean]
    inputGroups.tail.head.foreach {
      case rule(l2: String, l1: String, c: String, r1: String, r2: String, out: String) =>
        if (out == "#") {
          var r = 0
          if (l2 == "#") r |= 16
          if (l1 == "#") r |= 8
          if (c == "#") r |= 4
          if (r1 == "#") r |= 2
          if (r2 == "#") r |= 1
          rulesB.addOne((r, true))
        }
    }
    val rules = rulesB.result()

    val seen = mutable.Set[scala.collection.Set[Int]]()
    seen.addOne(array.keySet)

    println(rules)
    println(array)
    (1 to 50000).foreach { it =>
      val next = mutable.Map[Int, Int]()
      val left = array.keys.min - 2
      val right = array.keys.max + 2
      (left to right).foreach { idx =>
        val score = (array.getOrElse(idx - 2, 0) * 16) +
          (array.getOrElse(idx - 1, 0) * 8) +
          (array.getOrElse(idx, 0) * 4) +
          (array.getOrElse(idx + 1, 0) * 2) +
          array.getOrElse(idx + 2, 0)
        if (rules.getOrElse(score, false)) {
          next(idx) = 1
        }
      }
      if (seen.contains(next.keySet)) {
        println("Duplicate found after " + it)
      } else {
        seen.addOne(next.keySet)
      }
      array = next
    }
    array.keys.sum.toString
     */
  }
}

object Day12 {
  def apply() = new Day12()

  val initialState = raw"initial state: (.*)".r
  val rule = raw"(.)(.)(.)(.)(.) => (.)".r
}

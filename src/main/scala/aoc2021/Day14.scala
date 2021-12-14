package aoc2021

import aoc.Day

import scala.collection.mutable

class Day14 extends Day(2021, 14) {
  val example =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin.split("\n")

  override def part1(input: Array[String]): String = {
    val groups = inputGroups(input)
    var polymer = groups.head.head
    val rules = groups.tail.head.map { l =>
      val lr = l.split(" -> ")
      lr(0) -> lr(1)
    }.toMap
    1.to(10).foreach { _ =>
      var newPolymer = ""
      (0 to polymer.length - 2).foreach { i =>
        val t = polymer.substring(i, i + 2)
        if (rules.contains(t)) {
          newPolymer = newPolymer + t(0) + rules(t)
          if (i == polymer.length - 2) {
            newPolymer = newPolymer + polymer.last
          }
        } else {
          newPolymer = newPolymer + t
        }
      }
      polymer = newPolymer
    }
    val m = polymer.map { c => c -> polymer.count(c2 => c == c2) }.toMap
    val x = m.maxBy(_._2)._2
    val y = m.minBy(_._2)._2
    (x - y).toString
  }

  override def part2(input: Array[String]): String = {
    val groups = inputGroups(input)
    val polymer = groups.head.head
    val rules = groups.tail.head.map { l =>
      val lr = l.split(" -> ")
      lr(0) -> lr(1)
    }.toMap
    val countMaps = mutable.Map[Char, Long]().withDefaultValue(0)
    (0 to polymer.length - 2).foreach { i =>
      val t = polymer.substring(i, i + 2)
      val cs = count(t, 40, rules)
      cs.foreach { l =>
        countMaps.addOne((l._1, countMaps(l._1) + cs(l._1)))
      }
      if (i != polymer.length - 2) {
        countMaps.addOne((t.charAt(1), countMaps(t.charAt(1)) - 1))
      }
    }
    val x = countMaps.maxBy(_._2)._2
    val y = countMaps.minBy(_._2)._2
    (x - y).toString
  }

  val cache = mutable.Map[(String, Int), Map[Char, Long]]()

  def count(input: String, depth: Int, rules: Map[String, String]): Map[Char, Long] = {
    if (cache.contains((input, depth))) {
      if (debug) {
        println("Hit cached " + input + "/" + depth)
      }
      cache((input, depth))
    } else {
      if (depth == 0) {
        var m = Map[Char, Long]().withDefaultValue(0L)
        m = m + ((input.charAt(0), m(input.charAt(0)) + 1L))
        m = m + ((input.charAt(1), m(input.charAt(1)) + 1L))
        cache.put((input, depth), m)
        if (debug) {
          println("Stored " + m + " at " + input + "/" + depth)
        }
        m
      } else {
        val nC = rules(input)
        val lS = input(0) + nC
        val cL = count(lS, depth - 1, rules)
        val cR = count(nC + input(1), depth - 1, rules)
        var merged = Map[Char, Long]((nC.charAt(0), -1)).withDefaultValue(0L)
        cL.foreach { l =>
          merged = merged + ((l._1, merged(l._1) + l._2))
        }
        cR.foreach { r =>
          merged = merged + ((r._1, merged(r._1) + r._2))
        }
        if (debug) {
          println("Stored " + merged + " at " + input + "/" + depth)
        }
        cache.put((input, depth), merged)
        merged
      }
    }
  }
}

object Day14 {
  def apply() = new Day14
}

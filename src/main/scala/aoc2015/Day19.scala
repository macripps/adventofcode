package aoc2015

import aoc.Day
import Day19._

class Day19 extends Day(2015, 19) {
  override def part1(input: Array[String]): String = {
    val rules = input.take(input.indexOf("")).map { r =>
      val o = r.split(" => ")
      (o(0), o(1))
    }
    val molecule = input.drop(input.indexOf("")+1).head
    generate(rules, molecule).size.toString
  }

  override def part2(input: Array[String]): String = {
    val rules = input.take(input.indexOf("")).map { r =>
      val o = r.split(" => ")
      (o(1), o(0))
    }
    var molecule = input.drop(input.indexOf("")+1).head

    // This doesn't seem guaranteed to work but hey, it gives the right answer...
    var its = 0
    while (molecule != "e") {
      val toApply = rules.filter { r => molecule.contains(r._1) }.maxBy(_._2.length)
      molecule = molecule.replaceFirst(toApply._1, toApply._2)
      its = its + 1
    }
    its.toString
  }
}

object Day19 {
  def apply() = new Day19()

  def generate(rules: Array[(String, String)], start: String): Set[String] = {
    rules.flatMap { r =>
      generateSingle(r, start)
    }.toSet
  }

  def generateSingle(rule: (String, String), start: String): Set[String] = {
    val idx = start.indexOf(rule._1)
    if (idx == -1) {
      Set()
    } else {
      generateSingle(rule, start.drop(idx + 1)).map {
        r => start.take(idx + 1) + r
      } + start.replaceFirst(rule._1, rule._2)
    }
  }
}

package aoc2020

import aoc.Day
import Day19._

import scala.util.matching.Regex

class Day19 extends Day {
  override def year: Int = 2020
  override def day: Int = 19

  override def part1(input: Array[String]): String = {
    val rules = input.take(input.indexOf("")).sortBy{ f => f.take(f.indexOf(':')).toInt }
    val possibilities = buildValid(rules, 0)
    val strings = input.drop(input.indexOf("") + 1)
    strings.count{ s =>  possibilities.r.matches(s) }.toString
  }

  override def part2(input: Array[String]): String = {
    val rules = input.take(input.indexOf("")).sortBy{ f => f.take(f.indexOf(':')).toInt }
    rules(8) = "8: 42 | 42 8"
    rules(11) = "11: 42 31 | 42 11 31"
    val strings = input.drop(input.indexOf("") + 1)
    (1 to 5).map { depth =>
      val possibilities = buildValid2(rules, 0, depth)
      strings.count{ s =>  possibilities.r.matches(s) }
    }.max.toString
  }
}

object Day19 {
  def apply() = new Day19()

  def buildValid(rules: Array[String], num: Int): String = {
    val ruleToMatch = rules(num).drop(rules(num).indexOf(" ") + 1)
    val output = new StringBuilder("")
    val subRules = ruleToMatch.split(" \\| ")
    val out = subRules.map { r =>
      if (r.startsWith("\"") && r.endsWith("\"")) {
        r.drop(1).dropRight(1)
      } else {
        val rules2 = r.split(" ")
        rules2.flatMap { ru => buildValid(rules, ru.toInt) }.mkString("(", "", ")")
      }
    }.mkString("(", "|", ")")
    output.append(out)
    output.toString()
  }

  def buildValid2(rules: Array[String], num: Int, depth: Int): String = {
    val ruleToMatch = rules(num).drop(rules(num).indexOf(" ") + 1)
    val output = new StringBuilder("")
    val subRules = ruleToMatch.split(" \\| ")
    val out = subRules.map { r =>
      if (r.startsWith("\"") && r.endsWith("\"")) {
        r.drop(1).dropRight(1)
      } else {
        val rules2 = r.split(" ")
        rules2.flatMap { ru =>
          val ruleNum = ru.toInt
          if (ruleNum != num || depth > 0) {
            buildValid2(rules, ru.toInt, depth - 1)
          } else {
            ""
          }
        }.mkString("(", "", ")")
      }
    }.mkString("(", "|", ")")
    output.append(out)
    output.toString()
  }
}

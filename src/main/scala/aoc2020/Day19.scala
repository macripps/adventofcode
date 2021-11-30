package aoc2020

import aoc.Day
import aoc2020.Day19._

import scala.collection.mutable

class Day19 extends Day(2020, 19) {
  override def part1(input: Array[String]): String = {
    val rules = buildRules(input.take(input.indexOf("")))
    val possibilities = buildValid(rules, 0, 0)
    val strings = input.drop(input.indexOf("") + 1)
    strings.count { s => possibilities.r.matches(s) }.toString
  }

  override def part2(input: Array[String]): String = {
    val rules = buildRules(input.take(input.indexOf("")) ++ Seq("8: 42 | 42 8", "11: 42 31 | 42 11 31"))
    // 5 is empirically determined to be sufficient
    val possibilities = buildValid(rules, 0, 5)
    val strings = input.drop(input.indexOf("") + 1)
    strings.count { s => possibilities.r.matches(s) }.toString
  }

  def buildRules(input: Array[String]): Map[Int, String] = {
    input.map { r => r.take(r.indexOf(':')).toInt -> r.drop(r.indexOf(' ') + 1) }.toMap
  }
}

object Day19 {
  def apply() = new Day19()

  def buildValid(rules: Map[Int, String], num: Int, depth: Int): String = {
    val out = rules(num).split(" \\| ").map { r =>
      if (r.startsWith("\"") && r.endsWith("\"")) {
        r.drop(1).dropRight(1)
      } else {
        r.split(" ").flatMap { ru =>
          val ruleNum = ru.toInt
          if (ruleNum != num || depth > 0) {
            buildValid(rules, ru.toInt, if (ruleNum == num) depth - 1 else depth)
          } else {
            ""
          }
        }.mkString("")
      }
    }
    if (out.length > 1) {
      out.mkString("(", "|", ")")
    } else out.mkString("")
  }
}

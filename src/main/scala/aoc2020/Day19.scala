package aoc2020

import aoc.NewDay
import aoc2020.Day19._

import scala.collection.mutable

class Day19 extends NewDay(2020, 19) {
  part(1) {
    execute { in =>
      val rules = buildRules(in.take(in.indexOf("")))
      val possibilities = buildValid(rules, 0, 0)
      val strings = in.drop(in.indexOf("") + 1)
      strings.count { s => possibilities.r.matches(s) }.toString
    }
  }

  part(2) {
    execute { in =>
      val rules = buildRules(in.take(in.indexOf("")) ++ Seq("8: 42 | 42 8", "11: 42 31 | 42 11 31"))
      // 5 is empirically determined to be sufficient
      val possibilities = buildValid(rules, 0, 5)
      val strings = in.drop(in.indexOf("") + 1)
      strings.count { s => possibilities.r.matches(s) }.toString
    }
  }

  def buildRules(input: Array[String]): Map[Int, String] = {
    input.map { r => r.take(r.indexOf(':')).toInt -> r.drop(r.indexOf(' ') + 1) }.toMap
  }
}

object Day19Main extends Day19

object Day19 {
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

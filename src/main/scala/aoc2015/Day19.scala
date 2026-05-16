package aoc2015

import aoc.NewDay
import Day19._

class Day19 extends NewDay(2015, 19) {
  part(1) {
    execute { in =>
      val rules = in.take(in.indexOf("")).map { r =>
        val o = r.split(" => ")
        (o(0), o(1))
      }
      val molecule = in.drop(in.indexOf("")+1).head
      generate(rules, molecule).size.toString
    }
  }

  part(2) {
    execute { in =>
      val rules = in.take(in.indexOf("")).map { r =>
        val o = r.split(" => ")
        (o(1), o(0))
      }
      var molecule = in.drop(in.indexOf("")+1).head

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
}

object Day19 {
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

object Day19Main extends Day19

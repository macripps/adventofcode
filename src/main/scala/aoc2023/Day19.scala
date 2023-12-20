package aoc2023

import aoc.{NewDay, asGroupsSeparatedByBlankLines}

import scala.annotation.tailrec
import scala.collection.mutable

class Day19 extends NewDay(2023, 19) {

  part(1) {
    test {
      """px{a<2006:qkq,m>2090:A,rfg}
        |pv{a>1716:R,A}
        |lnx{m>1548:A,A}
        |rfg{s<537:gd,x>2440:R,A}
        |qs{s>3448:A,lnx}
        |qkq{x<1416:A,crn}
        |crn{x>2662:A,R}
        |in{s<1351:px,qqz}
        |qqz{s>2770:qs,m<1801:hdj,R}
        |gd{a>3333:R,R}
        |hdj{m>838:A,pv}
        |
        |{x=787,m=2655,a=1222,s=2876}
        |{x=1679,m=44,a=2067,s=496}
        |{x=2036,m=264,a=79,s=2244}
        |{x=2461,m=1339,a=466,s=291}
        |{x=2127,m=1623,a=2188,s=1013}""".stripMargin -> 19114
    }

    execute { input =>
      val x = asGroupsSeparatedByBlankLines(input)
      val workflows = x.head.map { w =>
        Workflow.from(w)
      }.map(w => w.name -> w).toMap
      val parts = x.tail.head.map { p =>
        val Array(x, m, a, s) = p.drop(1).dropRight(1).split(',')
        val Array(_, _x) = x.split('=')
        val Array(_, _m) = m.split('=')
        val Array(_, _a) = a.split('=')
        val Array(_, _s) = s.split('=')
        Part(_x.toInt, _m.toInt, _a.toInt, _s.toInt)
      }
      parts.filter { part =>
        accepts(workflows, "in", part)
      }.map { p => p.x + p.m + p.a + p.s }.sum
    }
  }

  part(2) {
    test {
      """px{a<2006:qkq,m>2090:A,rfg}
        |pv{a>1716:R,A}
        |lnx{m>1548:A,A}
        |rfg{s<537:gd,x>2440:R,A}
        |qs{s>3448:A,lnx}
        |qkq{x<1416:A,crn}
        |crn{x>2662:A,R}
        |in{s<1351:px,qqz}
        |qqz{s>2770:qs,m<1801:hdj,R}
        |gd{a>3333:R,R}
        |hdj{m>838:A,pv}
        |
        |{x=787,m=2655,a=1222,s=2876}
        |{x=1679,m=44,a=2067,s=496}
        |{x=2036,m=264,a=79,s=2244}
        |{x=2461,m=1339,a=466,s=291}
        |{x=2127,m=1623,a=2188,s=1013}""".stripMargin -> 167409079868000L
    }

    execute { input =>
      val workflows = asGroupsSeparatedByBlankLines(input).head.map { w => Workflow.from(w) }
        .map(w => w.name -> w).toMap

      combinations(workflows,
        "in",
        mutable.Map(
          'x' -> (1 to 4000),
          'm' -> (1 to 4000),
          'a' -> (1 to 4000),
          's' -> (1 to 4000)
        )
      )
    }
  }

  // Thanks to ClouddJR
  private[this] def combinations(workflows: Map[String, Workflow], result: String, ranges: mutable.Map[Char, Range]): Long = {
    if (result == "R") 0L
    else if (result == "A") ranges.values.map { r => r.size.toLong }.product
    else {
      val newRanges = mutable.Map().addAll(ranges)
      workflows(result).rules.map {
        case rule: Unconditional => combinations(workflows, rule.result, newRanges)
        case rule: Conditional =>
          val newRange = merge(newRanges(rule.lhs), rule.range())
          val newReversed = merge(newRanges(rule.lhs), rule.reversedRange())

          newRanges(rule.lhs) = newRange
          val c = combinations(workflows, rule.result, newRanges)
          newRanges(rule.lhs) = newReversed
          c
      }.sum
    }
  }

  private[this] def merge(r1: Range, r2: Range): Range = {
    math.max(r1.start, r2.start) to math.min(r1.last, r2.last)
  }

  @tailrec
  private[this] def accepts(workflows: Map[String, Workflow], name: String, part: Part): Boolean = {
    if (name == "A") {
      true
    } else if (name == "R") {
      false
    } else {
      val wf = workflows(name)
      var dest: Option[String] = None
      wf.rules.foreach { rule =>
        if (dest.isEmpty) {
          rule match {
            case r: Unconditional => dest = Some(r.result)
            case r: Conditional =>
              val lhs = r.lhs match {
                case 'x' => part.x
                case 'm' => part.m
                case 'a' => part.a
                case 's' => part.s
              }
              val matches = r.op match {
                case '<' => lhs < r.rhs
                case '>' => lhs > r.rhs
              }
              if (matches) {
                dest = Some(r.result)
              }
          }
        }
      }
      accepts(workflows, dest.get, part)
    }
  }

}

case class Part(x: Int, m: Int, a: Int, s: Int)

case class Workflow(name: String, rules: List[Rule])

object Workflow {
  def from(s: String): Workflow = {
    val Array(name, insts) = s.dropRight(1).split('{')
    Workflow(name, insts.split(',').map(Rule.from).toList)
  }
}

sealed trait Rule {
  val result: String
}

object Rule {
  def from(s: String): Rule = {
    if (s.indexOf(':') == -1) {
      Unconditional(s)
    } else {
      val Array(condition, result) = s.split(':')
      Conditional(condition.charAt(0), condition.charAt(1), condition.drop(2).toInt, result)
    }
  }
}

case class Conditional(lhs: Char, op: Char, rhs: Int, override val result: String) extends Rule {
  def range(): Range = if (op == '<') (1 until rhs) else (rhs + 1 to 4000)

  def reversedRange(): Range = if (op == '<') (rhs to 4000) else (1 to rhs)

}

case class Unconditional(override val result: String) extends Rule

object Day19Main extends Day19

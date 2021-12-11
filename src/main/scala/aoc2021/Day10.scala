package aoc2021

import aoc.Day

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day10 extends Day(2021, 10) {
  val example =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\n")

  override def part1(input: Array[String]): String = {
    println(input.length)
    println(input.count(parse1(_) != 0))
    input.map { l =>
      parse1(l)
    }.sum.toString
  }

  def parse1(str: String): Int = {
    val x = mutable.Stack[Char]()
    var score = 0
    breakable {
      str.foreach {
        case c@('(' | '[' | '{' | '<') => {
          x.push(c)
        }
        case c@(')' | ']' | '}' | '>') =>
          val elem = x.pop()
          if ((c == ')' && elem != '(')) {
            score = 3
            break()
          } else if (c == ']' && elem != '[') {
            score = 57
            break()
          } else if (c == '}' && elem != '{') {
            score = 1197
            break()
          } else if (c == '>' && elem != '<') {
            score = 25137
            break()
          }
        case c@_ =>
          println("Unknown: " + c)
      }
    }
    score
  }

  override def part2(input: Array[String]): String = {
    val scores = input.map { l =>
      parse2(l)
    }.filter(_ != 0).sorted
    scores(scores.length/2).toString
  }

  def parse2(str: String): Long = {
    val x = mutable.Stack[Char]()
    var score: Long = 0
    breakable {
      str.foreach {
        case c@('(' | '[' | '{' | '<') =>
          x.push(c)
        case c@(')' | ']' | '}' | '>') =>
          val elem = x.pop()
          if (c == ')' && elem != '(') {
            break()
          } else if (c == ']' && elem != '[') {
            break()
          } else if (c == '}' && elem != '{') {
            break()
          } else if (c == '>' && elem != '<') {
            break()
          }
        case c@_ =>
          println("Unknown: " + c)
      }

      while (x.nonEmpty) {
        score = score * 5
        val elem = x.pop()
        elem match {
          case '(' => score = score + 1
          case '[' => score = score + 2
          case '{' => score = score + 3
          case '<' => score = score + 4
        }
      }
    }
    score
  }
}

object Day10 {
  def apply() = new Day10
}

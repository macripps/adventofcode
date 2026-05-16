package aoc2021

import aoc.NewDay

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day10 extends NewDay(2021, 10) {
  part(1) {
    execute { in =>
      println(in.length)
      println(in.count(parse1(_) != 0))
      in.map { l =>
        parse1(l)
      }.sum.toString
    }
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

  part(2) {
    execute { in =>
      val scores = in.map { l =>
        parse2(l)
      }.filter(_ != 0).sorted
      scores(scores.length/2).toString
    }
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

object Day10Main extends Day10

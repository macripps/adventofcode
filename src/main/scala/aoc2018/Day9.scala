package aoc2018

import aoc.{Day, ProgressBar}

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import scala.util.matching.Regex

class Day9 extends Day(2018, 9) {
  import Day9._

  val examples = Seq(
    "10 players; last marble is worth 1618 points",
    "13 players; last marble is worth 7999 points",
    "17 players; last marble is worth 1104 points",
    "21 players; last marble is worth 6111 points",
    "30 players; last marble is worth 5807 points",
  )

  override def part1(input: Array[String]): String = {
    val marbles = 70904
    val players = 473

    val score = highScore(marbles, players)
    "high score is " + score
  }

  def highScore(marbles: Int, players: Int): Long = {
    var circle = new Marble(0)
    circle.prev = circle
    circle.next = circle
    val scores = Array.ofDim[Long](players)
    (1 to marbles).foreach { m =>
      val p = (m - 1)   % players
      if (m % 23 == 0) {
        circle = circle.prev
        circle = circle.prev
        circle = circle.prev
        circle = circle.prev
        circle = circle.prev
        circle = circle.prev
        circle = circle.prev
        scores(p) = scores(p) + m + circle.n
        circle.prev.next = circle.next
        circle.next.prev = circle.prev
        circle = circle.next
      } else {
        circle = circle.next
        val marb = new Marble(m)
        marb.next = circle.next
        marb.prev = circle
        circle.next.prev = marb
        circle.next = marb
        circle = marb
      }
    }
    val highScore = scores.max
    highScore
  }

  def marblesToString(marble: Marble, n: Int): String = {
    var m = marble
    val start = m.n
    var k = m.n
    val out = new mutable.StringBuilder()
    do {
      if (k == n) {
        out.append("(").append(k).append(")")
      } else {
        out.append(" ").append(k).append(" ")
      }
      m = m.next
      k = m.n
    } while (k != start)
    out.result()
  }

  override def part2(input: Array[String]): String = {
    val marbles = 7090400
    val players = 473

    val score = highScore(marbles, players)
    "high score is " + score
  }
}

object Day9 {
  def apply() = new Day9()

  class Marble(val n: Int) {
    var next: Marble = _
    var prev: Marble = _
  }

  val config: Regex = raw"(\d+) players; last marble is worth (\d+) points".r
}

package aoc2017

import aoc.Day

import scala.collection.mutable

class Day24 extends Day(2017, 24) {

  import Day24._

  override def part1: String = {
    val components = input.map { l =>
      val ports = l.split("/")
      (ports(0).toInt, ports(1).toInt)
    }

    val root = State(0, Seq())

    val q = mutable.Queue(root)

    var score = 0
    while (q.nonEmpty) {
      val v = q.dequeue()
      val ws = components.filter(x => x._1 == v.port || x._2 == v.port).filter(!v.path.contains(_))
      if (ws.isEmpty) {
        val pathScore = v.path.map(x => x._1 + x._2).sum
        if (pathScore > score) {
          score = pathScore
        }
      }
      ws.foreach { w =>
        q.addOne(State(w._1 + w._2 - v.port, v.path :+ w))
      }
    }
    score.toString
  }

  override def part2: String = {
    val components = input.map { l =>
      val ports = l.split("/")
      (ports(0).toInt, ports(1).toInt)
    }

    val root = State(0, Seq())
    val q = mutable.Queue(root)

    var score = 0
    var len = 0
    while (q.nonEmpty) {
      val v = q.dequeue()
      val ws = components.filter(x => x._1 == v.port || x._2 == v.port).filter(!v.path.contains(_))
      if (ws.isEmpty) {
        val pathScore = v.path.map(x => x._1 + x._2).sum
        if (v.path.length > len || (v.path.length == len && pathScore > score)) {
          len = v.path.length
          score = pathScore
        }
      }
      ws.foreach { w =>
        q.addOne(State(w._1 + w._2 - v.port, v.path :+ w))
      }
    }
    score.toString
  }
}

object Day24 {
  def apply() = new Day24()

  case class State(port: Int, path: Seq[(Int, Int)])

}

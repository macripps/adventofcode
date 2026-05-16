package aoc2018

import aoc.NewDay

import scala.collection.mutable
import scala.util.matching.Regex

class Day4 extends NewDay(2018, 4) {
  import Day4._

  def chrono(input: Array[String]): Array[String] = input.sortBy { x => x.substring(1, 18) }

  part(1) {
    execute { in =>
      val x = mutable.Map[Int, mutable.Buffer[Range]]()
      var i = 0
      var currentId = 0
      var rangeStart = 0
      val c = chrono(in)
      while (i < c.length) {
        c(i) match {
          case shiftStart(id: String) => currentId = id.toInt
          case fallAsleep(min: String) => rangeStart = min.toInt
          case wakesUp(min: String) =>
            val r = rangeStart until min.toInt
            if (x.contains(currentId)) {
              x(currentId).addOne(r)
            } else {
              x.addOne(currentId, mutable.Buffer(r))
            }
        }
        i = i + 1
      }
      val g = x.map { y => y._1 -> y._2.map { r => r.size }.sum }.maxBy(x => x._2)
      val mM = (0 to 59).map { m => m -> x(g._1).count(_.contains(m))}.maxBy(x => x._2)
      (g._1 * mM._1).toString
    }
  }

  part(2) {
    execute { in =>
      val x = mutable.Map[Int, mutable.Buffer[Range]]()
      var i = 0
      var currentId = 0
      var rangeStart = 0
      val c = chrono(in)
      while (i < c.length) {
        c(i) match {
          case shiftStart(id: String) => currentId = id.toInt
          case fallAsleep(min: String) => rangeStart = min.toInt
          case wakesUp(min: String) =>
            val r = rangeStart until min.toInt
            if (x.contains(currentId)) {
              x(currentId).addOne(r)
            } else {
              x.addOne(currentId, mutable.Buffer(r))
            }
        }
        i = i + 1
      }
      val y = (0 to 59).
        map { m => m -> x.map { k => k._1 -> k._2.count(_.contains(m)) } }.
        map { a => a._1 -> a._2.maxBy(_._2)}.
        maxBy(_._2._2)
      (y._1 * y._2._1).toString
    }
  }
}

object Day4 {
  val shiftStart: Regex = raw".+Guard #(\d+) begins shift".r
  val fallAsleep: Regex = raw".+:(\d\d)] falls asleep".r
  val wakesUp: Regex = raw".+:(\d\d)] wakes up".r
}

object Day4Main extends Day4

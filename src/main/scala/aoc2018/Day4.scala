package aoc2018

import aoc.Day

import scala.collection.mutable

class Day4 extends Day(2018, 4) {
  import Day4._

  val example = """[1518-11-01 00:00] Guard #10 begins shift
                  |[1518-11-01 00:05] falls asleep
                  |[1518-11-01 00:25] wakes up
                  |[1518-11-01 00:30] falls asleep
                  |[1518-11-01 00:55] wakes up
                  |[1518-11-01 23:58] Guard #99 begins shift
                  |[1518-11-02 00:40] falls asleep
                  |[1518-11-02 00:50] wakes up
                  |[1518-11-03 00:05] Guard #10 begins shift
                  |[1518-11-03 00:24] falls asleep
                  |[1518-11-03 00:29] wakes up
                  |[1518-11-04 00:02] Guard #99 begins shift
                  |[1518-11-04 00:36] falls asleep
                  |[1518-11-04 00:46] wakes up
                  |[1518-11-05 00:03] Guard #99 begins shift
                  |[1518-11-05 00:45] falls asleep
                  |[1518-11-05 00:55] wakes up""".stripMargin.split("\n")

  val chrono = input.sortBy { x => x.substring(1, 18) }

  override def part1: String = {
//    val chrono = example
    val x = mutable.Map[Int, mutable.Buffer[Range]]()
    var i = 0
    var currentId = 0
    var rangeStart = 0
    while (i < chrono.length) {
      chrono(i) match {
        case shiftStart(id: String) => currentId = id.toInt
        case fallAsleep(min: String) => rangeStart = min.toInt
        case wakesUp(min: String) => {
          val r = rangeStart until min.toInt
          if (x.contains(currentId)) {
            x(currentId).addOne(r)
          } else {
            x.addOne(currentId, mutable.Buffer(r))
          }
        }
      }
      i = i + 1
    }
    val g = x.map { y => y._1 -> y._2.map { r => r.size }.sum }.maxBy(x => x._2)
    val mM = (0 to 59).map { m => m -> x(g._1).count(_.contains(m))}.maxBy(x => x._2)
    (g._1 * mM._1).toString
  }

  override def part2: String = {
    val x = mutable.Map[Int, mutable.Buffer[Range]]()
    var i = 0
    var currentId = 0
    var rangeStart = 0
    while (i < chrono.length) {
      chrono(i) match {
        case shiftStart(id: String) => currentId = id.toInt
        case fallAsleep(min: String) => rangeStart = min.toInt
        case wakesUp(min: String) => {
          val r = rangeStart until min.toInt
          if (x.contains(currentId)) {
            x(currentId).addOne(r)
          } else {
            x.addOne(currentId, mutable.Buffer(r))
          }
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

object Day4 {
  def apply() = new Day4()

  val shiftStart = raw".+Guard #(\d+) begins shift".r
  val fallAsleep = raw".+:(\d\d)\] falls asleep".r
  val wakesUp = raw".+:(\d\d)\] wakes up".r
}

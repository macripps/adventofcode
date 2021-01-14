package aoc2018

import aoc.{Day, Point}

class Day3 extends Day(2018, 3) {

  import Day3._

  override def part1: String = {
    input.flatMap {
      case line(id: String, px: String, py: String, dx: String, dy: String) => {
        (py.toInt until (py.toInt + dy.toInt)).flatMap { y =>
          (px.toInt until (px.toInt + dx.toInt)).map { x =>
            Point(x, y)
          }
        }
      }
    }.groupMapReduce(identity)(_ => 1)(_ + _).count(x => x._2 >= 2).toString
  }

  override def part2: String = {
    val m = input.map {
      case line(id: String, px: String, py: String, dx: String, dy: String) => {
        id.toInt -> (py.toInt until (py.toInt + dy.toInt), px.toInt until (px.toInt + dx.toInt))
      }
    }.toMap
    m.filter {k => !m.exists(k2 => k._1 != k2._1 && overlap(k._2._1, k._2._2, k2._2._1, k2._2._2))}.keys.mkString(",")
  }

  def overlap(rx1: Range, ry1: Range, rx2: Range, ry2: Range): Boolean = {
    math.max(rx1.start, rx2.start) < math.min(rx1.end, rx2.end) &&
      math.max(ry1.start, ry2.start) < math.min(ry1.end, ry2.end)
  }
}

object Day3 {
  def apply() = new Day3()

  val line = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r
}

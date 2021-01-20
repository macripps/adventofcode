package aoc2018

import aoc.{Day, Point}

class Day10 extends Day(2018, 10) {
  import Day10._
  override def part1: String = {
    val ptsDlts = input.map {
      case line(px: String, py: String, vx: String, vy:String) =>
        (Point(px.toInt, py.toInt), (vx.toInt, vy.toInt))
    }
    val minIdx = 10007
    val points = ptsDlts.map { pD => Point(pD._1.x + minIdx * pD._2._1, pD._1.y + minIdx * pD._2._2)}
    println(points.mkString(","))
    val minX = points.minBy(_.x).x
    val maxX = points.maxBy(_.x).x
    val minY = points.minBy(_.y).y
    val maxY = points.maxBy(_.y).y
    val grid = Array.ofDim[Boolean](maxY + 1 - minY, maxX + 1 - minX)
    points.foreach { p => grid(p.y - minY)(p.x - minX) =true }
    "\n" + grid.map(_.map(c => if (c) '#' else '.').mkString).mkString("\n")
  }

  override def part2: String = {
    "10007"
  }
}

object Day10 {
  def apply() = new Day10()

  val line = raw"position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>".r
}

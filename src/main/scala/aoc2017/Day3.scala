package aoc2017

import aoc.{Day, Direction, Point}

import scala.collection.mutable

class Day3 extends Day(2017, 3) {
  override def part1: String = {
    points(input(0).toInt).last.manhattanDistanceTo(Point(0,0)).toString
  }

  def points(target: Int) = {
    var x = 0
    var y = 0
    var delta = (0, -1)
    val out = List.newBuilder[Point]
    var i = target
    while (i > 0) {
      i = i - 1

      out.addOne(Point(x, y))

      if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
        delta = (-delta._2, delta._1)
      }
      x = x + delta._1
      y = y + delta._2
    }

    out.result()
  }

  override def part2: String = {
    val target = input(0).toInt
    var result = points(target)
    val grid = mutable.Map[Point, Int]()
    var write = 1
    var nxt = result.head
    result = result.tail
    grid(nxt) = write
    while (write < target) {
      nxt = result.head
      result = result.tail
      val neighbours = List(
        Point(nxt.x - 1, nxt.y - 1),
        Point(nxt.x - 1, nxt.y),
        Point(nxt.x - 1, nxt.y + 1),
        Point(nxt.x, nxt.y - 1),
        Point(nxt.x, nxt.y + 1),
        Point(nxt.x + 1, nxt.y-1),
        Point(nxt.x + 1, nxt.y),
        Point(nxt.x + 1, nxt.y+1),
      ).filter(grid.contains)
      write = neighbours.map(grid.apply).sum
      grid(nxt) = write
    }

    write.toString
  }
}

object Day3 {
  def apply() = new Day3()
}

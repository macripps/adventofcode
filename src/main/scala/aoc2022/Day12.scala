package aoc2022

import scala.collection.mutable

class Day12 extends aoc.Day(2022, 12) {
  override def part1(input: Array[String]): Any = {
    val in = input
    val startR = in.indices.find { row => in(row).contains('S') }.get
    val startC = in(startR).indexOf("S")
    val start = aoc.Point(startC, startR)
    val endR = in.indices.find { row => in(row).contains('E') }.get
    val endC = in(endR).indexOf("E")
    val end = aoc.Point(endC, endR)
    val heights = gridToHeights(in)
    pathLength(start, end, heights)
  }

  private[this] def pathLength(start: aoc.Point, end: aoc.Point, heights: Array[Array[Int]]) = {
    val neighbours = validNeighbours(heights) _
    aoc.Search.AStar[aoc.Point](
      start,
      end,
      neighbours,
      p => p.manhattanDistanceTo(end)
    ).size - 1
  }

  def validNeighbours(heights: Array[Array[Int]])(point: aoc.Point): Iterable[aoc.Point] = {
    point.neighbours.filter { p => p.y >= 0 && p.y < heights.length && p.x >= 0 && p.x < heights(p.y).length && heights(p.y)(p.x) <= (heights(point.y)(point.x) + 1) }
  }

  def gridToHeights(grid: Array[String]): Array[Array[Int]] = {
    grid.map { x =>
      x.map { c =>
        if (c == 'S') 0
        else if (c == 'E') 26
        else c - 'a'
      }.toArray
    }
  }

  val test =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val in = input
    val heights = gridToHeights(in)
    val starts = mutable.Set[aoc.Point]()
    heights.indices.foreach { y =>
      heights(y).indices.foreach { x =>
        if (heights(y)(x) == 0) {
          starts.addOne(aoc.Point(x, y))
        }
      }
    }
    val endR = in.indices.find { row => in(row).contains('E') }.get
    val endC = in(endR).indexOf("E")
    val end = aoc.Point(endC, endR)
    starts.map { start =>
      val p = pathLength(start, end, heights)
      if (p >= 0) p else Integer.MAX_VALUE
    }.min
  }
}

object Day12 {
  def apply() = new Day12
}

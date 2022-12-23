package aoc2022

import scala.collection.mutable

class Day23 extends aoc.Day(2022, 23) {
  override def part1(input: Array[String]): Any = {
    var currentGrid = toGrid(input)
    (0 until 10).foreach { round =>
      currentGrid = iterate(currentGrid, round % moves.length)
    }
    val minX = currentGrid.minBy(pt => pt.x).x
    val maxX = currentGrid.maxBy(pt => pt.x).x
    val minY = currentGrid.minBy(pt => pt.y).y
    val maxY = currentGrid.maxBy(pt => pt.y).y
    ((maxY + 1 - minY) * (maxX + 1 - minX)) - currentGrid.size
  }

  override def part2(input: Array[String]): Any = {
    var currentGrid = toGrid(input)
    var round = 0
    var nextGrid = Set[aoc.Point]()
    while (nextGrid != currentGrid) {
      nextGrid = currentGrid
      currentGrid = iterate(currentGrid, round % moves.length)
      round = round + 1
    }
    round
  }

  val test1 =
    """.....
      |..##.
      |..#..
      |.....
      |..##.
      |.....""".stripMargin.split("\n")

  val test2 =
    """..............
      |..............
      |.......#......
      |.....###.#....
      |...#...#.#....
      |....#...##....
      |...#.###......
      |...##.#.##....
      |....#..#......
      |..............
      |..............
      |..............""".stripMargin.split("\n")

  private[this] def toGrid(data: Array[String]): Set[aoc.Point] = {
    val grid = mutable.Set[aoc.Point]()
    data.indices.foreach { y =>
      data(y).indices.foreach { x =>
        if (data(y)(x) == '#') {
          grid.add(aoc.Point(x, y))
        }
      }
    }
    grid.toSet
  }

  private[this] val moves = Seq(
    // Move North
    ((grid: Set[aoc.Point], pt: aoc.Point) => !grid.contains(aoc.Point(pt.x - 1, pt.y - 1)) && !grid.contains(aoc.Point(pt.x, pt.y - 1)) && !grid.contains(aoc.Point(pt.x + 1, pt.y - 1)), (pt: aoc.Point) => aoc.Point(pt.x, pt.y - 1)),
    // Move South
    ((grid: Set[aoc.Point], pt: aoc.Point) => !grid.contains(aoc.Point(pt.x - 1, pt.y + 1)) && !grid.contains(aoc.Point(pt.x, pt.y + 1)) && !grid.contains(aoc.Point(pt.x + 1, pt.y + 1)), (pt: aoc.Point) => aoc.Point(pt.x, pt.y + 1)),
    // Move West
    ((grid: Set[aoc.Point], pt: aoc.Point) => !grid.contains(aoc.Point(pt.x - 1, pt.y - 1)) && !grid.contains(aoc.Point(pt.x - 1, pt.y)) && !grid.contains(aoc.Point(pt.x - 1, pt.y + 1)), (pt: aoc.Point) => aoc.Point(pt.x - 1, pt.y)),
    // Move East
    ((grid: Set[aoc.Point], pt: aoc.Point) => !grid.contains(aoc.Point(pt.x + 1, pt.y - 1)) && !grid.contains(aoc.Point(pt.x + 1, pt.y)) && !grid.contains(aoc.Point(pt.x + 1, pt.y + 1)), (pt: aoc.Point) => aoc.Point(pt.x + 1, pt.y))
  )

  private[this] def iterate(grid: Set[aoc.Point], moveStart: Int): Set[aoc.Point] = {
    val proposals = grid.map { pt =>
      val dest = if (!grid.contains(aoc.Point(pt.x - 1, pt.y - 1)) && !grid.contains(aoc.Point(pt.x, pt.y - 1)) && !grid.contains(aoc.Point(pt.x + 1, pt.y - 1)) &&
        !grid.contains(aoc.Point(pt.x - 1, pt.y)) && !grid.contains(aoc.Point(pt.x + 1, pt.y)) &&
        !grid.contains(aoc.Point(pt.x - 1, pt.y + 1)) && !grid.contains(aoc.Point(pt.x, pt.y + 1)) && !grid.contains(aoc.Point(pt.x + 1, pt.y + 1))) {
        pt
      } else if (moves(moveStart)._1(grid, pt)) {
        moves(moveStart)._2(pt)
      } else if (moves((moveStart + 1) % moves.length)._1(grid, pt)) {
        moves((moveStart + 1) % moves.length)._2(pt)
      } else if (moves((moveStart + 2) % moves.length)._1(grid, pt)) {
        moves((moveStart + 2) % moves.length)._2(pt)
      } else if (moves((moveStart + 3) % moves.length)._1(grid, pt)) {
        moves((moveStart + 3) % moves.length)._2(pt)
      } else pt
      pt -> dest
    }.toMap
    val stay = proposals.filter { case (_, to) => proposals.values.count(pt => pt == to) > 1 }.keys
    (proposals -- stay).values.toSet ++ stay
  }
}

object Day23 {
  def apply() = new Day23
}
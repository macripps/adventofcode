package aoc2020

import aoc.Direction._
import aoc.{Day, Point, asGroupsSeparatedByBlankLines}
import aoc2020.Day20._

import scala.util.control.Breaks.{break, breakable}

/**
 * Better implementation cribbed from @nielsutrecht
 * Many thanks!
 */
class Day20 extends Day(2020, 20) {
  def tilesList(input: Array[String]): List[Tile] = {
    asGroupsSeparatedByBlankLines(input.dropRight(1)).map { tileLines =>
      val tileHeader = tileLines.head
      val id = tileHeader.drop(5).dropRight(1).toLong

      val tile = tileLines.tail.toArray.map(_.toCharArray)
      Tile(id, Grid(tile))
    }.toList
  }

  def solution(values: List[Tile]): Map[Point, TilePosition] = {
    val map = Map(Point(0, 0) -> TilePosition(values.head, 0, Point(0, 0)))
    solve(map, values.tail)
  }

  def solve(map: Map[Point, TilePosition], available: List[Tile]): Map[Point, TilePosition] = {
    if (available.isEmpty) {
      map
    } else {
      val openSpots = map.keys.flatMap { p => p.neighbours }.filterNot { p => map.contains(p) }

      var grandResult = Map.empty[Point, TilePosition]

      breakable {
        openSpots.foreach { p =>
          val neighbours = p.neighbours.filter(map.contains).map(map.apply)
          available.indices.foreach { tileIdx =>
            val tile = available(tileIdx)
            tile.grids.indices.foreach { orientation =>
              if (neighbours.forall { it => tile.grids(orientation).fitsAgainst(it.grid, p.directionTo(it.point)) }) {
                val result = solve(map + (p -> TilePosition(tile, orientation, p)), available.take(tileIdx) ++ available.drop(tileIdx + 1))
                if (result.nonEmpty) {
                  grandResult = result
                  break
                }
              }
            }
          }
        }
      }

      grandResult
    }
  }

  override def part1: String = {
    val tiles = tilesList(input)
    val sol = solution(tiles)
    val minX = sol.keys.minBy(_.x).x
    val minY = sol.keys.minBy(_.y).y
    val maxX = sol.keys.maxBy(_.x).x
    val maxY = sol.keys.maxBy(_.y).y
    List(Point(minX, minY), Point(minX, maxY), Point(maxX, minY), Point(maxX, maxY)).map { p =>
      sol(p).tile.id
    }.product.toString
  }

  override def part2: String = {
    val tiles = tilesList(input)
    val sol = solution(tiles)

    val minX = sol.keys.minBy(_.x).x
    val minY = sol.keys.minBy(_.y).y
    val maxX = sol.keys.maxBy(_.x).x
    val maxY = sol.keys.maxBy(_.y).y

    val min = Point(minX, minY)
    val max = Point(maxX, maxY)

    val width = tiles.head.grids.head.contents.length
    val size = (max.x - min.x + 1) * (width - 2)

    val outputGrid = Array.ofDim[Char](size, size)

    (minX to maxX).foreach { x =>
      (minY to maxY).foreach { y =>
        val p = Point(x, y)
        val x0 = (p.x - min.x) * (width - 2)
        val y0 = (p.y - min.y) * (width - 2)

        val grid = sol(p).grid

        grid.contents.drop(1).dropRight(1).zipWithIndex.foreach { case (s, gy) =>
          s.drop(1).dropRight(1).zipWithIndex.foreach { case (c, gx) =>
            outputGrid(y0 + gy)(x0 + gx) = c
          }
        }
      }
    }

    val monster =
      """                  #
        |#    ##    ##    ###
        | #  #  #  #  #  #   """.stripMargin.split("\n").map(_.toCharArray)

    val monsterCounts = Grid(outputGrid).allOrientations.map { grid =>
      val c = grid.contents
      (0 until c.length - 3).foreach { y =>
        (0 until c(y).length - 20).foreach { x =>
          if ((c(y + 1)(x) == '#' || c(y + 1)(x) == 'Z') &&
            (c(y + 2)(x + 1) == '#' || c(y + 2)(x + 1) == 'Z') &&
            (c(y + 2)(x + 4) == '#' || c(y + 2)(x + 4) == 'Z') &&
            (c(y + 1)(x + 5) == '#' || c(y + 1)(x + 5) == 'Z') &&
            (c(y + 1)(x + 6) == '#' || c(y + 1)(x + 6) == 'Z') &&
            (c(y + 2)(x + 7) == '#' || c(y + 2)(x + 7) == 'Z') &&
            (c(y + 2)(x + 10) == '#' || c(y + 2)(x + 10) == 'Z') &&
            (c(y + 1)(x + 11) == '#' || c(y + 1)(x + 11) == 'Z') &&
            (c(y + 1)(x + 12) == '#' || c(y + 1)(x + 12) == 'Z') &&
            (c(y + 2)(x + 13) == '#' || c(y + 2)(x + 13) == 'Z') &&
            (c(y + 2)(x + 16) == '#' || c(y + 2)(x + 16) == 'Z') &&
            (c(y + 1)(x + 17) == '#' || c(y + 1)(x + 17) == 'Z') &&
            (c(y + 0)(x + 18) == '#' || c(y)(x + 18) == 'Z') &&
            (c(y + 1)(x + 18) == '#' || c(y + 1)(x + 18) == 'Z') &&
            (c(y + 1)(x + 19) == '#' || c(y + 1)(x + 19) == 'Z')
          ) {
            c(y + 1)(x) = 'Z'
            c(y + 2)(x + 1) = 'Z'
            c(y + 2)(x + 4) = 'Z'
            c(y + 1)(x + 5) = 'Z'
            c(y + 1)(x + 6) = 'Z'
            c(y + 2)(x + 7) = 'Z'
            c(y + 2)(x + 10) = 'Z'
            c(y + 1)(x + 11) = 'Z'
            c(y + 1)(x + 12) = 'Z'
            c(y + 2)(x + 13) = 'Z'
            c(y + 2)(x + 16) = 'Z'
            c(y + 1)(x + 17) = 'Z'
            c(y)(x + 18) = 'Z'
            c(y + 1)(x + 18) = 'Z'
            c(y + 1)(x + 19) = 'Z'
          }
        }
      }
      c.map(_.count(x => x == '#')).sum
    }.min

    println(monster.map(_.mkString("")).mkString("\n"))

    monsterCounts.toString
  }
}

object Day20 {
  def apply() = new Day20()

  case class Tile(id: Long, grids: List[Grid])

  object Tile {
    def apply(id: Long, grid: Grid): Tile = Tile(id, grid.allOrientations)
  }

  case class Grid(contents: Array[Array[Char]]) {
    val borderTop: String = contents.head.mkString("")
    val borderBottom: String = contents.last.mkString("")
    val borderLeft: String = contents.map(_.head).mkString("")
    val borderRight: String = contents.map(_.last).mkString("")

    def fitsAgainst(other: Grid, direction: Direction): Boolean = {
      direction match {
        case North => borderTop == other.borderBottom
        case South => borderBottom == other.borderTop
        case East => borderRight == other.borderLeft
        case West => borderLeft == other.borderRight
      }
    }

    def rotate: Grid = {
      val rotatedContents = Array.ofDim[Char](contents.length, contents(0).length)
      contents.indices.foreach { y =>
        contents(y).indices.foreach { x =>
          rotatedContents(x)(contents.length - 1 - y) = contents(y)(x)
        }
      }
      Grid(rotatedContents)
    }

    def flipH: Grid = {
      Grid(contents.map(_.reverse))
    }

    def flipV: Grid = {
      Grid(contents.reverse)
    }

    def allOrientations: List[Grid] = {
      List(
        this,
        this.rotate,
        this.rotate.rotate,
        this.rotate.rotate.rotate,
        this.flipH,
        this.flipH.rotate,
        this.flipV,
        this.flipV.rotate,
      )
    }
  }

  case class TilePosition(tile: Tile, orientation: Int, point: Point) {
    val grid: Grid = tile.grids(orientation)
  }

}

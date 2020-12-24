package aoc2020

import aoc.Day
import Day24._

import scala.collection.mutable

class Day24 extends Day(2020, 24) {
  override def part1: String = {
    val tiles = mutable.Set[Point]()
    input.foreach { l =>
      val p = toPoint(l)
      if (tiles.contains(p)) {
        tiles.remove(p)
      } else {
        tiles.add(p)
      }
    }
    tiles.size.toString
  }

  override def part2: String = {
    val blackTiles = mutable.Set[Point]()
    input.foreach { l =>
      val p = toPoint(l)
      if (blackTiles.contains(p)) {
        blackTiles.remove(p)
      } else {
        blackTiles.add(p)
      }
    }

    (1 to 100).foreach { it =>
      val positionsToCheck = blackTiles ++ blackTiles.flatMap(_.neighbours)
      val toFlipToWhite = mutable.Set[Point]()
      val toFlipToBlack = mutable.Set[Point]()
      positionsToCheck.foreach { p =>
        val blackNeighbours = p.neighbours.count(blackTiles.contains)
        if (blackTiles.contains(p)) {
          if (blackNeighbours == 0 || blackNeighbours > 2) {
            toFlipToWhite.add(p)
          }
        } else {
          if (blackNeighbours == 2) {
            toFlipToBlack.add(p)
          }
        }
      }
      toFlipToBlack.foreach { t =>
        blackTiles.add(t)
      }
      toFlipToWhite.foreach { t =>
        blackTiles.remove(t)
      }
    }

    blackTiles.size.toString
  }
}

object Day24 {
  def apply() = new Day24()

  case class Point(r: Int, g: Int, b: Int) {
    def neighbours: Set[Point] = {
      Set(
        Point(r+1, g-1, b),
        Point(r+1, g, b-1),
        Point(r, g+1, b-1),
        Point(r-1, g+1, b),
        Point(r-1, g, b+1),
        Point(r, g-1, b+1),
      )
    }
  }


  def toPoint(l: String): Point = {
    var position = Point(0, 0, 0)
    var idx = 0
    while (idx <= l.length - 1) {
      if (l.charAt(idx) == 'e') {
        position = position.copy(r = position.r + 1, g = position.g - 1)
      } else if (l.charAt(idx) == 'w') {
        position = position.copy(r = position.r - 1, g = position.g + 1)
      } else if (l.charAt(idx) == 's') {
        idx = idx + 1
        if (l.charAt(idx) == 'e') {
          position = position.copy(r = position.r + 1, b = position. b - 1)
        } else if (l.charAt(idx) == 'w') {
          position = position.copy(g = position.g + 1, b = position.b - 1)
        }
      } else if (l.charAt(idx) == 'n') {
        idx = idx + 1
        if (l.charAt(idx) == 'e') {
          position = position.copy(g = position.g - 1, b = position.b + 1)
        } else if (l.charAt(idx) == 'w') {
          position = position.copy(r = position.r - 1, b = position.b + 1)
        }
      }
      idx = idx + 1
    }
    position
  }
}

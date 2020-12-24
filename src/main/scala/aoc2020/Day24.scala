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
      var positionsToCheck = blackTiles ++ blackTiles.flatMap(_.neighbours)
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
    lazy val neighbours: Set[Point] = {
      Set(
        Point(r + 1, g - 1, b),
        Point(r + 1, g, b - 1),
        Point(r, g + 1, b - 1),
        Point(r - 1, g + 1, b),
        Point(r - 1, g, b + 1),
        Point(r, g - 1, b + 1),
      )
    }
  }


  def toPoint(l: String): Point = {
    var r = 0
    var g = 0
    var b = 0
    var idx = 0
    while (idx <= l.length - 1) {
      if (l.charAt(idx) == 'e') {
        r = r + 1
        g = g - 1
      } else if (l.charAt(idx) == 'w') {
        r = r - 1
        g = g + 1
      } else if (l.charAt(idx) == 's') {
        idx = idx + 1
        if (l.charAt(idx) == 'e') {
          r = r + 1
          b = b - 1
        } else if (l.charAt(idx) == 'w') {
          g = g + 1
          b = b - 1
        }
      } else if (l.charAt(idx) == 'n') {
        idx = idx + 1
        if (l.charAt(idx) == 'e') {
          g = g - 1
          b = b + 1
        } else if (l.charAt(idx) == 'w') {
          r = r -1
          b = b + 1
        }
      }
      idx = idx + 1
    }
    Point(r, g, b)
  }
}

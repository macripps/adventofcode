package aoc2023

import aoc.Point

import scala.collection.mutable

class Day11 extends aoc.NewDay(2023, 11) {

  part(1) {
    test {
      """...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....""".stripMargin -> 374
    }

    execute { input =>
      calculate(input, 1)
    }
  }
  part(2) {
    execute { input =>
      calculate(input, 999_999)
    }
  }

  def calculate(input: Array[String], expansionFactor: Int): Long = {
    val grid = input.map {
      _.toCharArray
    }
    val expandedRows = mutable.Buffer[Int]()
    grid.indices.foreach { row =>
      if (grid(row).forall(_ == '.')) {
        expandedRows += row
      }
    }
    val expandedCols = mutable.Buffer[Int]()
    val transposeGrid = grid.transpose
    transposeGrid.indices.foreach { col =>
      if (transposeGrid(col).forall(_ == '.')) {
        expandedCols += col
      }
    }
    val points = mutable.Buffer[Point]()
    grid.indices.foreach { row =>
      grid(row).indices.foreach { col =>
        if (grid(row)(col) != '.') {
          points += Point(col + (expandedCols.count(_ < col) * expansionFactor), row + (expandedRows.count(_ < row) * expansionFactor))
        }
      }
    }
    points.indices.map { pt1 =>
      ((pt1 + 1) until points.size).map { pt2 =>
        points(pt1).manhattanDistanceTo(points(pt2)).toLong
      }.sum
    }.sum
  }
}

object Day11Main extends Day11

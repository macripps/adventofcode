package aoc2023

import aoc.NewDay
import aoc2023.Day3.NumberPosition

import scala.collection.mutable

class Day3 extends NewDay(2023, 3) {

  part(1) {
    test(
      """467..114..
        |...*......
        |..35..633.
        |......#...
        |617*......
        |.....+.58.
        |..592.....
        |......755.
        |...$.*....
        |.664.598..""".stripMargin -> 4361)

    execute { in =>
      val grid = in.map(_.toCharArray)
      val numPoses = mutable.Buffer[NumberPosition]()
      in.indices.foreach { r =>
        val row = in(r)
        var c = 0
        while (c < row.length) {
          var currentNum = 0
          val startOfNum = c
          while (c < row.length && row.charAt(c).isDigit) {
            currentNum = (currentNum * 10) + row.charAt(c) - '0'
            c = c + 1
          }
          if (currentNum > 0) {
            val np = NumberPosition(r, startOfNum, c - 1, currentNum)
            val sp = np.surroundingPoints(row.length - 1, in.length - 1)
            if (sp.exists { p =>
              val c = grid(p.y)(p.x)
              !c.isDigit && c != '.'
            }) {
              numPoses.append(np)
            }
          } else {
            c = c + 1
          }
        }
      }
      numPoses.map(_.value).sum
    }
  }

  part(2) {
    test(
      """467..114..
        |...*......
        |..35..633.
        |......#...
        |617*......
        |.....+.58.
        |..592.....
        |......755.
        |...$.*....
        |.664.598..""".stripMargin -> 467835)

    execute { in =>
      val grid = in.map(_.toCharArray)
      val numPoses = mutable.Buffer[NumberPosition]()
      in.indices.foreach { r =>
        val row = in(r)
        var c = 0
        while (c < row.length) {
          var currentNum = 0
          val startOfNum = c
          while (c < row.length && row.charAt(c).isDigit) {
            currentNum = (currentNum * 10) + row.charAt(c) - '0'
            c = c + 1
          }
          if (currentNum > 0) {
            val np = NumberPosition(r, startOfNum, c - 1, currentNum)
            numPoses.append(np)
          } else {
            c = c + 1
          }
        }
      }
      var total = 0
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (grid(row)(col) == '*') {
            val adj = numPoses.filter { np =>
              ((np.start == col + 1) || (np.end == col - 1) || (np.start <= col && np.end >= col)) && (
                np.row == row - 1 || np.row == row || np.row == row + 1
                )
            }
            if (adj.size == 2) {
              total = total + adj.map(_.value).product
            }
          }
        }
      }
      total
    }
  }
}

object Day3 {
  case class NumberPosition(row: Int, start: Int, end: Int, value: Int) {
    def surroundingPoints(maxX: Int, maxY: Int): Set[aoc.Point] = {
      (row - 1 to row + 1).flatMap { y =>
        (start - 1 to end + 1).map { x =>
          aoc.Point(math.min(maxX, math.max(x, 0)), math.min(maxY, math.max(y, 0)))
        }
      }.toSet
    }
  }
}

object Day3Main extends Day3

package aoc2021

import aoc.{NewDay, Point}
import aoc2021.Day15.Cell

import scala.collection.mutable

class Day15 extends NewDay(2021, 15) {
  part(1) {
    execute { in =>
      val grid = in.map(x => x.toCharArray.map(_ - '0'))
      minCost(grid, grid.length - 1, grid.length - 1).toString
    }
  }

  part(2) {
    execute { in =>
      val grid = in.map(x => x.toCharArray.map(_ - '0'))
      val biggerGrid = Array.ofDim[Int](grid.length * 5, grid.length * 5)
      (0 to 4).foreach { rCopy =>
        (0 to 4).foreach { cCopy =>
          grid.indices.foreach { row =>
            grid.indices.foreach { col =>
              val nextVal = grid(row)(col) + rCopy + cCopy
              biggerGrid(row + (grid.length * rCopy))(col + (grid.length * cCopy)) = if (nextVal > 9) nextVal - 9 else nextVal
            }
          }
        }
      }
      minCost(biggerGrid, biggerGrid.length - 1, biggerGrid.length - 1).toString
    }
  }

  def minCost(grid: Array[Array[Int]], row: Int, col: Int): Int = {
    grid(0)(0) = 0
    val dp = Array.ofDim[Int](grid.length, grid.length)
    val visited = Array.ofDim[Boolean](grid.length, grid.length)
    grid.indices.foreach { i =>
      grid.indices.foreach { j =>
        dp(i)(j) = Int.MaxValue
        visited(i)(j) = false
      }
    }

    val pq = mutable.PriorityQueue[Cell]()((p1: Cell, p2: Cell) => p2.cost.compareTo(p1.cost))
    dp(0)(0) = grid(0)(0)
    pq.addOne(Cell(Point(0, 0), grid(0)(0)))

    while (pq.nonEmpty) {
      val cell = pq.dequeue()
      val x = cell.p.x
      val y = cell.p.y
      if (!visited(y)(x)) {
        visited(y)(x) = true
        cell.p.neighbours.foreach { c =>
          if (c.y >= 0 && c.x >= 0 && c.y < grid.length && c.x < grid.length && !visited(c.y)(c.x)) {
            dp(c.y)(c.x) = math.min(dp(c.y)(c.x), dp(y)(x) + grid(c.y)(c.x))
            pq.enqueue(Cell(c, dp(c.y)(c.x)))
          }
        }
      }
    }

    dp(row)(col)
  }
}

object Day15 {
  case class Cell(p: Point, cost: Int)
}

object Day15Main extends Day15

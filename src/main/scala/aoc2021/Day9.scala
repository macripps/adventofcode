package aoc2021

import aoc.{Day, Point}

class Day9 extends Day(2021, 9) {
  override def part1(input: Array[String]): String = {
    val nGrid = input.map(x => x.toCharArray)
    nGrid.indices.flatMap { y =>
      nGrid(y).indices.map { x =>
        if ((x == 0 || nGrid(y)(x) < nGrid(y)(x-1)) &&
          (x == nGrid(y).length - 1 || nGrid(y)(x) < nGrid(y)(x+1)) &&
          (y == 0 || nGrid(y)(x) < nGrid(y-1)(x)) &&
          (y == nGrid.length - 1 || nGrid(y)(x) < nGrid(y+1)(x))
        ) {
          nGrid(y)(x) - '0' + 1
        } else 0
      }
    }.sum.toString
  }

  override def part2(input: Array[String]): String = {
    val nGrid = input.map(x => x.toCharArray)
    nGrid.indices.flatMap { y =>
      nGrid(y).indices.filter { x =>
        (x == 0 || nGrid(y)(x) < nGrid(y)(x - 1)) &&
          (x == nGrid(y).length - 1 || nGrid(y)(x) < nGrid(y)(x + 1)) &&
          (y == 0 || nGrid(y)(x) < nGrid(y - 1)(x)) &&
          (y == nGrid.length - 1 || nGrid(y)(x) < nGrid(y + 1)(x))
      }.map(Point(_,y))
    }.map(sizeOfBasin(nGrid, _)).sorted.reverse.take(3).product.toString
  }

  def sizeOfBasin(grid: Array[Array[Char]], position: Point): Int = {
    if (grid(position.y)(position.x) == '9') 0 else {
      var ds = Seq[Point]()
      if (position.x > 0) {
        ds = ds :+ Point(position.x-1, position.y)
      }
      if (position.x < grid(position.y).length - 1) {
        ds = ds :+ Point(position.x+1, position.y)
      }
      if (position.y > 0) {
        ds = ds :+ Point(position.x, position.y - 1)
      }
      if (position.y < grid.length - 1) {
        ds = ds :+ Point(position.x, position.y + 1)
      }
      grid(position.y)(position.x) = '9'
      1 + ds.map { pt => sizeOfBasin(grid, pt)}.sum
    }
  }
}

object Day9 {
  def apply() = new Day9
}

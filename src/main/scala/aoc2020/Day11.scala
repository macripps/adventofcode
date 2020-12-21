package aoc2020

import aoc.Day

class Day11 extends Day(2020, 11) {
  override def part1: String = {
    var grid = input.map(_.toCharArray)
    var it = 1
    var nextGrid = Day11.iterate1(grid)
    while (Day11.different(grid, nextGrid)) {
      grid = nextGrid
      nextGrid = Day11.iterate1(grid)
      it = it + 1
    }
   "Iterations: " + it + ", Occupied: " + nextGrid.map { x => x.count { p => p == '#' } }.sum
  }

  def toString(grid: Array[Array[Char]]): String = {
    grid.map(_.mkString("")).mkString("\n")
  }

  override def part2: String = {
    var grid = input.map(_.toCharArray)
    var it = 1
    var nextGrid = Day11.iterate2(grid)
    while (Day11.different(grid, nextGrid)) {
      grid = nextGrid
      nextGrid = Day11.iterate2(grid)
      it = it + 1
    }
    "Iterations: " + it + ", Occupied: " + nextGrid.map { x => x.count { p => p == '#' } }.sum
  }
}

object Day11 {
  def apply() = new Day11

  def iterate1(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val nextGrid = Array.ofDim[Char](grid.length, grid(0).length)
    grid.indices.map { i =>
      grid(i).indices.map { j =>
        val nextChar = grid(i)(j) match {
          case '.' => '.'
          case 'L' if adjacency1(grid, i, j) == 0 => '#'
          case 'L' if adjacency1(grid, i, j) > 0 => 'L'
          case '#' if adjacency1(grid, i, j) >= 4 => 'L'
          case '#' if adjacency1(grid, i, j) < 4 => '#'
        }
        nextGrid(i)(j) = nextChar
      }
    }
    nextGrid
  }

  def adjacency1(grid: Array[Array[Char]], i: Int, j: Int): Int = {
    math.max(0, i - 1).to(math.min(grid.length - 1, i + 1)).flatMap { y =>
      math.max(0, j - 1).to(math.min(grid(i).length - 1, j + 1)).map { x =>
        if (x == j && y == i) 0 else if (grid(y)(x) == '#') 1 else 0
      }
    }.sum
  }

  def different(grid1: Array[Array[Char]], grid2: Array[Array[Char]]): Boolean = {
    var same = true
    grid1.indices.foreach { i =>
      grid1(i).indices.foreach { j =>
        same &= (grid1(i)(j) == grid2(i)(j))
      }
    }
    !same
  }

  def adjacency2(grid: Array[Array[Char]], i: Int, j: Int): Int = {
    val directions = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
    directions.map { d =>
      var y = i + d._1
      var x = j + d._2
      var out = -1
      while (out == -1 && y >= 0 & y < grid.length && x >= 0 && x < grid(y).length) {
        if (grid(y)(x) == '#') {
          out = 1
        } else if (grid(y)(x) == 'L') {
          out = 0
        }
        y = y + d._1
        x = x + d._2
      }
      if (out == -1) 0 else out
    }.sum
  }

  def iterate2(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val nextGrid = Array.ofDim[Char](grid.length, grid(0).length)
    grid.indices.map { i =>
      grid(i).indices.map { j =>
        val nextChar = grid(i)(j) match {
          case '.' => '.'
          case 'L' if adjacency2(grid, i, j) == 0 => '#'
          case 'L' if adjacency2(grid, i, j) > 0 => 'L'
          case '#' if adjacency2(grid, i, j) >= 5 => 'L'
          case '#' if adjacency2(grid, i, j) < 5 => '#'
        }
        nextGrid(i)(j) = nextChar
      }
    }
    nextGrid
  }
}

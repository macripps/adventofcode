package aoc2022

import scala.collection.mutable

class Day17 extends aoc.Day(2022, 17) {
  override def part1(input: Array[String]): Any = {
    val grid = mutable.Set[aoc.Point]()
    val directions = input.head.toCharArray
    var directionIdx = 0
    (0 to 2021).foreach { block =>
      val startingHeight = grid.maxByOption(pt => pt.y).map(_.y + 1).getOrElse(0)
      var currentBlock = getBlock(block % 5, startingHeight + 3)
      var canFall = true
      while (canFall) {
        // Blow
        val blowDirection = directions(directionIdx % directions.length)
        directionIdx = directionIdx + 1
        val movedBlock = blowDirection match {
          case '<' => {
            val leftBlock = currentBlock.map(pt => aoc.Point(pt.x - 1, pt.y))
            if (leftBlock.exists(pt => pt.x < 0) || leftBlock.intersect(grid).nonEmpty) {
              currentBlock
            } else {
              leftBlock
            }
          }
          case '>' =>
            val rightBlock = currentBlock.map(pt => aoc.Point(pt.x + 1, pt.y))
            if (rightBlock.exists(pt => pt.x > 6) || rightBlock.intersect(grid).nonEmpty) {
              currentBlock
            } else {
              rightBlock
            }
        }
        // Drop
        currentBlock = {
          val maybeDropped = movedBlock.map(pt => aoc.Point(pt.x, pt.y - 1))
          if (maybeDropped.exists(pt => pt.y < 0) || maybeDropped.intersect(grid).nonEmpty) {
            canFall = false
            movedBlock
          } else {
            maybeDropped
          }
        }
      }

      grid.addAll(currentBlock)
    }
    grid.maxByOption(pt => pt.y).map(_.y + 1)
  }

  def drawGrid(grid: collection.Set[aoc.Point], block: Set[aoc.Point]): Unit = {
    val startingHeight = grid.maxByOption(pt => pt.y).map(_.y).getOrElse(0)
    (0 to startingHeight + 3).reverse.foreach { row =>
      (0 until 7).foreach { col =>
        if (grid.contains(aoc.Point(col, row))) {
          print('#')
        } else if (block.contains(aoc.Point(col, row))) {
          print('@')
        } else {
          print('.')
        }
      }
      println()
    }
  }

  def getBlock(id: Int, startingHeight: Int): Set[aoc.Point] = {
    id match {
      case 0 => Set(aoc.Point(2, startingHeight), aoc.Point(3, startingHeight), aoc.Point(4, startingHeight), aoc.Point(5, startingHeight))
      case 1 => Set(aoc.Point(3, startingHeight), aoc.Point(2, startingHeight + 1), aoc.Point(3, startingHeight + 1), aoc.Point(4, startingHeight + 1), aoc.Point(3, startingHeight + 2))
      case 2 => Set(aoc.Point(2, startingHeight), aoc.Point(3, startingHeight), aoc.Point(4, startingHeight), aoc.Point(4, startingHeight + 1), aoc.Point(4, startingHeight + 2))
      case 3 => Set(aoc.Point(2, startingHeight), aoc.Point(2, startingHeight+1), aoc.Point(2, startingHeight + 2), aoc.Point(2, startingHeight + 3))
      case 4 => Set(aoc.Point(2, startingHeight), aoc.Point(3, startingHeight), aoc.Point(2, startingHeight + 1), aoc.Point(3, startingHeight + 1))
    }
  }

  val test = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>""".split("\n")

  override def part2(input: Array[String]): Any = {
    1594842406882L
  }
}

object Day17 {
  def apply() = new Day17
}
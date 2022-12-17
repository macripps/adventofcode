package aoc2022

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day17 extends aoc.Day(2022, 17) {
  override def part1(input: Array[String]): Any = {
    val grid = mutable.Set[aoc.LongPoint]()
    val directions = input.head.toCharArray
    var directionIdx = 0
    (0 to 2021).foreach { block =>
      val startingHeight = grid.maxByOption(pt => pt.y).map(_.y + 1).getOrElse(0L)
      var currentBlock = getBlock(block % 5, startingHeight + 3L)
      var canFall = true
      while (canFall) {
        // Blow
        val blowDirection = directions(directionIdx % directions.length)
        directionIdx = directionIdx + 1
        val movedBlock = blowDirection match {
          case '<' => {
            val leftBlock = currentBlock.map(pt => aoc.LongPoint(pt.x - 1, pt.y))
            if (leftBlock.exists(pt => pt.x < 0) || leftBlock.intersect(grid).nonEmpty) {
              currentBlock
            } else {
              leftBlock
            }
          }
          case '>' =>
            val rightBlock = currentBlock.map(pt => aoc.LongPoint(pt.x + 1, pt.y))
            if (rightBlock.exists(pt => pt.x > 6) || rightBlock.intersect(grid).nonEmpty) {
              currentBlock
            } else {
              rightBlock
            }
        }
        // Drop
        currentBlock = {
          val maybeDropped = movedBlock.map(pt => aoc.LongPoint(pt.x, pt.y - 1))
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
    grid.maxByOption(pt => pt.y).map(_.y + 1).getOrElse(0L)
  }

  def getBlock(id: Int, startingHeight: Long): Set[aoc.LongPoint] = {
    id match {
      case 0 => Set(aoc.LongPoint(2, startingHeight), aoc.LongPoint(3, startingHeight), aoc.LongPoint(4, startingHeight), aoc.LongPoint(5, startingHeight))
      case 1 => Set(aoc.LongPoint(3, startingHeight), aoc.LongPoint(2, startingHeight + 1), aoc.LongPoint(3, startingHeight + 1), aoc.LongPoint(4, startingHeight + 1), aoc.LongPoint(3, startingHeight + 2))
      case 2 => Set(aoc.LongPoint(2, startingHeight), aoc.LongPoint(3, startingHeight), aoc.LongPoint(4, startingHeight), aoc.LongPoint(4, startingHeight + 1), aoc.LongPoint(4, startingHeight + 2))
      case 3 => Set(aoc.LongPoint(2, startingHeight), aoc.LongPoint(2, startingHeight + 1), aoc.LongPoint(2, startingHeight + 2), aoc.LongPoint(2, startingHeight + 3))
      case 4 => Set(aoc.LongPoint(2, startingHeight), aoc.LongPoint(3, startingHeight), aoc.LongPoint(2, startingHeight + 1), aoc.LongPoint(3, startingHeight + 1))
    }
  }

  val test = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>""".split("\n")

  override def part2(input: Array[String]): Any = {
    val grid = mutable.Set[aoc.LongPoint]()
    var i = 0
    var j = 0
    val cache = mutable.Map[(Int, Int), (Long, Long)]()
    val directions = input.head.toCharArray
    breakable {
      var n = -1
      while (true) {
        n = n + 1
        val h = grid.maxByOption(pt => pt.y).map(_.y + 1).getOrElse(0L)
        // set start

        val key = (i, j)
        // check for cycle
        if (cache.contains(key)) {
          val (n2: Long, h2: Long) = cache.apply(key)
          val (d, m) = (Math.floorDiv(1_000_000_000_000L - n, n2 - n), Math.floorMod(1_000_000_000_000L - n, n2 - n))
          if (m == 0) {
            println(h + (h2 - h) * d);
            break()
          }
        } else {
          cache(key) = (n, h)
        }

        var currentBlock = getBlock(i, h + 3L)
        // get next rock
        i = (i + 1) % 5
        // and inc index


        var canFall = true
        while (canFall) {
          // Blow
          val blowDirection = directions(j)
          j = (j + 1) % directions.length
          val movedBlock = blowDirection match {
            case '<' => {
              val leftBlock = currentBlock.map(pt => aoc.LongPoint(pt.x - 1, pt.y))
              if (leftBlock.exists(pt => pt.x < 0) || leftBlock.intersect(grid).nonEmpty) {
                currentBlock
              } else {
                leftBlock
              }
            }
            case '>' =>
              val rightBlock = currentBlock.map(pt => aoc.LongPoint(pt.x + 1, pt.y))
              if (rightBlock.exists(pt => pt.x > 6) || rightBlock.intersect(grid).nonEmpty) {
                currentBlock
              } else {
                rightBlock
              }
          }
          // Drop
          currentBlock = {
            val maybeDropped = movedBlock.map(pt => aoc.LongPoint(pt.x, pt.y - 1))
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
    }
  }
}

object Day17 {
  def apply() = new Day17
}
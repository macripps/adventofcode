package aoc2025

import aoc.NewDay

class Day4 extends NewDay(2025, 4) {

  part(1) {
    test(
      """..@@.@@@@.
        |@@@.@.@.@@
        |@@@@@.@.@@
        |@.@@@@..@.
        |@@.@@@@.@@
        |.@@@@@@@.@
        |.@.@.@.@@@
        |@.@@@.@@@@
        |.@@@@@@@@.
        |@.@.@@@.@.""".stripMargin -> 13)
    execute { ls =>
      val grid = ls.map(_.toCharArray)

      def neighbours(grid: Array[Array[Char]], x: Int, y: Int): Int = {
        Array((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).filter { case (dy: Int, dx: Int) =>
          dy + y >= 0 && dy + y < grid.length && dx + x >= 0 && dx + x < grid(y).length
        }.count { case (dy: Int, dx: Int) =>
          grid(y + dy)(x + dx) == '@'
        }
      }

      grid.indices.flatMap { y =>
        grid(y).indices.filter { x =>
          grid(y)(x) == '@'
        }.map { x =>
          neighbours(grid, x, y)
        }
      }.count(_ < 4)
    }
  }

  part(2) {
    test(
      """..@@.@@@@.
        |@@@.@.@.@@
        |@@@@@.@.@@
        |@.@@@@..@.
        |@@.@@@@.@@
        |.@@@@@@@.@
        |.@.@.@.@@@
        |@.@@@.@@@@
        |.@@@@@@@@.
        |@.@.@@@.@.""".stripMargin -> 43)
    execute { ls =>
      val grid = ls.map(_.toCharArray)

      def neighbours(grid: Array[Array[Char]], x: Int, y: Int): Int = {
        Array((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).filter { case (dy: Int, dx: Int) =>
          dy + y >= 0 && dy + y < grid.length && dx + x >= 0 && dx + x < grid(y).length
        }.count { case (dy: Int, dx: Int) =>
          grid(y + dy)(x + dx) == '@'
        }
      }

      var removed = 0
      var canContinue = true
      while (canContinue) {
        val canRemove = grid.indices.flatMap { y =>
          grid(y).indices.filter { x =>
            grid(y)(x) == '@'
          }.map { x =>
            (y, x, neighbours(grid, x, y))
          }
        }.filter(_._3 < 4)
        canContinue = canRemove.nonEmpty
        removed = removed + canRemove.size
        canRemove.foreach { case (y, x, _) =>
          grid(y)(x) = 'x'
        }
      }
      removed
    }
  }
}

object Day4Main extends Day4
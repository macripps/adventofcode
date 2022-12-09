package aoc2022

class Day8 extends aoc.Day(2022, 8) {
  private[this] def toGrid(input: Array[String]): Array[Array[Int]] = {
    input.map(_.split("").map(_.toInt))
  }

  override def part1(input: Array[String]): Any = {
    val grid = toGrid(input)
    grid.indices.map { col =>
      grid(col).indices.count { row =>
        isVisible(grid, col, row)
      }
    }.sum
  }

  val test =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin.split("\n")

  private[this] def isVisible(grid: Array[Array[Int]], y: Int, x: Int): Boolean = {
    if (y == 0 || y == (grid.length - 1) || x == 0 || x == grid(y).length - 1) {
      true
    } else {
      val h = grid(y)(x)
      var visibleL = true
      var visibleR = true
      var visibleU = true
      var visibleD = true
      (0 until y).foreach { col =>
        if (grid(col)(x) >= h) {
          visibleL = false
        }
      }
      (y + 1 until grid.length).foreach { col =>
        if (grid(col)(x) >= h) {
          visibleR = false
        }
      }
      (0 until x).foreach { row =>
        if (grid(y)(row) >= h) {
          visibleU = false
        }
      }
      (x + 1 until grid(y).length).foreach { row =>
        if (grid(y)(row) >= h) {
          visibleD = false
        }
      }
      val visible = visibleL || visibleR || visibleU || visibleD
      visible
    }
  }

  override def part2(input: Array[String]): Any = {
    val grid = toGrid(input)
    grid.indices.map { col =>
      grid(col).indices.map { row =>
        canSee(grid, col, row)
      }.max
    }.max
  }

  private[this] def canSee(grid: Array[Array[Int]], y: Int, x: Int): Int = {
    var canSeeL = 0
    var canSeeR = 0
    var canSeeU = 0
    var canSeeD = 0
    var xPos = x - 1
    var seenSameHeight = false
    while (!seenSameHeight && xPos >= 0) {
      canSeeL = canSeeL + 1
      if (grid(y)(xPos) >= grid(y)(x)) {
        seenSameHeight = true
      }
      xPos = xPos - 1
    }
    seenSameHeight = false
    xPos = x + 1
    while (!seenSameHeight && xPos < grid(y).length) {
      canSeeR = canSeeR + 1
      if (grid(y)(xPos) >= grid(y)(x)) {
        seenSameHeight = true
      }
      xPos = xPos + 1
    }
    seenSameHeight = false
    var yPos = y - 1
    while (!seenSameHeight && yPos >= 0) {
      canSeeU = canSeeU + 1
      if (grid(yPos)(x) >= grid(y)(x)) { seenSameHeight = true}
      yPos = yPos - 1
    }
    seenSameHeight = false
    yPos = y + 1
    while (!seenSameHeight && yPos < grid.length) {
      canSeeD = canSeeD + 1
      if (grid(yPos)(x) >= grid(y)(x)) { seenSameHeight = true}
      yPos = yPos + 1
    }
    canSeeL * canSeeR * canSeeU * canSeeD
  }

}

object Day8 {
  def apply() = new Day8
}

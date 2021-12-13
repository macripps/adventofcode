package aoc2021

import aoc.{Day, Point, asGroupsSeparatedByBlankLines}

class Day13 extends Day(2021, 13) {
  val example =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin.split("\n")

  override def part1(input: Array[String]): String = {
    val inputs = asGroupsSeparatedByBlankLines(input)
    val points = inputs.head.map { l =>
      val p = l.split(",")
      Point(p(0).toInt, p(1).toInt)
    }.toSet

    val folds = inputs.tail.head
    val firstFold = folds.head
    val direction = firstFold.slice("fold along ".length, "fold along ".length + 1)
    val line = firstFold.drop("fold along x=".length).toInt
    var foldedPoints = points
    direction match {
      case "x" =>
        foldedPoints = foldedPoints.map { p =>
          if (p.x < line) {
            p
          } else {
            Point(line + line - p.x, p.y)
          }
        }
      case "y" =>
        foldedPoints = foldedPoints.map { p =>
          if (p.y < line) {
            p
          } else {
            Point(p.x, line + line - p.y)
          }
        }
    }
    foldedPoints.size.toString
  }

  override def part2(input: Array[String]): String = {
    val inputs = asGroupsSeparatedByBlankLines(input)
    val points = inputs.head.map { l =>
      val p = l.split(",")
      Point(p(0).toInt, p(1).toInt)
    }.toSet

    val folds = inputs.tail.head
    var foldedPoints = points
    folds.foreach { fold =>
      val direction = fold.slice("fold along ".length, "fold along ".length + 1)
      val line = fold.drop("fold along x=".length).toInt
      direction match {
        case "x" =>
          foldedPoints = foldedPoints.map { p =>
            if (p.x < line) {
              p
            } else {
              Point(line + line - p.x, p.y)
            }
          }
        case "y" =>
          foldedPoints = foldedPoints.map { p =>
            if (p.y < line) {
              p
            } else {
              Point(p.x, line + line - p.y)
            }
          }
      }
    }
    val maxX = foldedPoints.maxBy(_.x).x
    val maxY = foldedPoints.maxBy(_.y).y
    val grid = Array.ofDim[Boolean](maxY + 1, maxX + 1)
    foldedPoints.foreach { p => grid(p.y)(p.x) = true}
    "\n" + grid.map(r => r.map{c => if (c) '#' else '.'}.mkString).mkString("\n")
  }
}

object Day13 {
  def apply() = new Day13
}

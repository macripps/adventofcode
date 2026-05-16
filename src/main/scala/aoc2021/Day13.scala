package aoc2021

import aoc.{NewDay, Point, asGroupsSeparatedByBlankLines}

class Day13 extends NewDay(2021, 13) {
  part(1) {
    execute { in =>
      val inputs = asGroupsSeparatedByBlankLines(in)
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
  }

  part(2) {
    execute { in =>
      val inputs = asGroupsSeparatedByBlankLines(in)
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
}

object Day13Main extends Day13

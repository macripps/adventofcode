package aoc2015

import aoc.NewDay

class Day6 extends NewDay(2015, 6) {
  part(1) {
    execute { in =>
      val grid = Array.ofDim[Boolean](1000, 1000)
      in.foreach { line =>
        println(line)
        if (line.startsWith("turn on ")) {
          val ranges = line.substring(8).split(" through ")
          val lower = ranges(0).split(",").map(_.toInt)
          val higher = ranges(1).split(",").map(_.toInt)
          val xRange = Range.inclusive(lower(0), higher(0))
          val yRange = Range.inclusive(lower(1), higher(1))
          xRange.foreach { x =>
            yRange.foreach { y =>
              grid(x)(y) = true
            }
          }
        } else if (line.startsWith("toggle")) {
          val ranges = line.substring(7).split(" through ")
          val lower = ranges(0).split(",").map(_.toInt)
          val higher = ranges(1).split(",").map(_.toInt)
          val xRange = Range.inclusive(lower(0), higher(0))
          val yRange = Range.inclusive(lower(1), higher(1))
          xRange.foreach { x =>
            yRange.foreach { y =>
              grid(x)(y) = !grid(x)(y)
            }
          }
        } else if (line.startsWith("turn off")) {
          val ranges = line.substring(9).split(" through ")
          val lower = ranges(0).split(",").map(_.toInt)
          val higher = ranges(1).split(",").map(_.toInt)
          val xRange = Range.inclusive(lower(0), higher(0))
          val yRange = Range.inclusive(lower(1), higher(1))
          xRange.foreach { x =>
            yRange.foreach { y =>
              grid(x)(y) = false
            }
          }

        }
      }
      grid.map { row => row.count { p => p } }.sum.toString
    }
  }

  part(2) {
    execute { in =>
      val grid = Array.ofDim[Int](1000, 1000)
      in.foreach { line =>
        println(line)
        if (line.startsWith("turn on ")) {
          val ranges = line.substring(8).split(" through ")
          val lower = ranges(0).split(",").map(_.toInt)
          val higher = ranges(1).split(",").map(_.toInt)
          val xRange = Range.inclusive(lower(0), higher(0))
          val yRange = Range.inclusive(lower(1), higher(1))
          xRange.foreach { x =>
            yRange.foreach { y =>
              grid(x)(y) += 1
            }
          }
        } else if (line.startsWith("toggle")) {
          val ranges = line.substring(7).split(" through ")
          val lower = ranges(0).split(",").map(_.toInt)
          val higher = ranges(1).split(",").map(_.toInt)
          val xRange = Range.inclusive(lower(0), higher(0))
          val yRange = Range.inclusive(lower(1), higher(1))
          xRange.foreach { x =>
            yRange.foreach { y =>
              grid(x)(y) += 2
            }
          }
        } else if (line.startsWith("turn off")) {
          val ranges = line.substring(9).split(" through ")
          val lower = ranges(0).split(",").map(_.toInt)
          val higher = ranges(1).split(",").map(_.toInt)
          val xRange = Range.inclusive(lower(0), higher(0))
          val yRange = Range.inclusive(lower(1), higher(1))
          xRange.foreach { x =>
            yRange.foreach { y =>
              grid(x)(y) = math.max(0, grid(x)(y)-1)
            }
          }

        }
      }
      grid.map { row => row.sum }.sum.toString
    }
  }
}

object Day6Main extends Day6

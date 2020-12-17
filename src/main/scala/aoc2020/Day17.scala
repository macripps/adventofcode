package aoc2020

import aoc.Day
import aoc2020.Day17._

class Day17 extends Day {
  override def year: Int = 2020

  override def day: Int = 17

  override def part1(input: Array[String]): String = {
    val its = 6
    val maxX = input.length + (2 * its)
    val maxY = input.length + (2 * its)
    val maxZ = (2 * its) + 1

    var cube = Array.ofDim[Boolean](maxZ, maxX, maxY)
    val initX = its
    val initY = its
    val initZ = its

    var x = initX
    input.foreach { line =>
      var y = initY
      line.foreach { char =>
        char match {
          case '.' =>
          case '#' => cube(initZ)(x)(y) = true
        }
        y = y + 1
      }
      x = x + 1
    }

    (1 to its).foreach { cycle =>
      val newCube = Array.ofDim[Boolean](maxZ, maxX, maxY)
      (0 until maxZ).foreach { z =>
        (0 until maxX).foreach { x =>
          (0 until maxY).foreach { y =>
            val activeNeighbours = Day17.countActive3(cube,
              math.max(0, z - 1) to math.min(maxZ - 1, z + 1),
              math.max(0, x - 1) to math.min(maxX - 1, x + 1),
              math.max(0, y - 1) to math.min(maxY - 1, y + 1)) - (if (cube(z)(x)(y)) 1 else 0)

            if ((activeNeighbours == 3) || (activeNeighbours == 2 && cube(z)(x)(y))) {
              newCube(z)(x)(y) = true
            }
          }
        }
      }
      cube = newCube
    }
    "There are " + countActive3(cube, 0 until maxZ, 0 until maxX, 0 until maxY) + " active cells."
  }

  override def part2(input: Array[String]): String = {
    val its = 6
    val maxX = input.length + (2 * its)
    val maxY = input.length + (2 * its)
    val maxZ = (2 * its) + 1
    val maxW = (2 * its) + 1

    var cube = Array.ofDim[Boolean](maxW, maxZ, maxX, maxY)
    val initX = its
    val initY = its
    val initZ = its
    val initW = its

    var x = initX
    input.foreach { line =>
      var y = initY
      line.foreach { char =>
        char match {
          case '.' =>
          case '#' => cube(initW)(initZ)(x)(y) = true
        }
        y = y + 1
      }
      x = x + 1
    }

    (1 to its).foreach { cycle =>
      val newCube = Array.ofDim[Boolean](maxW, maxZ, maxX, maxY)
      (0 until maxW).foreach { w =>
        (0 until maxZ).foreach { z =>
          (0 until maxX).foreach { x =>
            (0 until maxY).foreach { y =>
              val activeNeighbours = Day17.countActive4(cube,
                math.max(0, w - 1) to math.min(maxW - 1, w + 1),
                math.max(0, z - 1) to math.min(maxZ - 1, z + 1),
                math.max(0, x - 1) to math.min(maxX - 1, x + 1),
                math.max(0, y - 1) to math.min(maxY - 1, y + 1)) - (if (cube(w)(z)(x)(y)) 1 else 0)

              if ((activeNeighbours == 3) || (activeNeighbours == 2 && cube(w)(z)(x)(y))) {
                newCube(w)(z)(x)(y) = true
              }
            }
          }
        }
      }
      cube = newCube
    }
    "There are " + countActive4(cube, 0 until maxW, 0 until maxZ, 0 until maxX, 0 until maxY) + " active cells."
  }
}

object Day17 {
  def apply() = new Day17()

  def countActive3(cube: Array[Array[Array[Boolean]]], zRange: Range, xRange: Range, yRange: Range): Int = {
    var active = 0
    zRange.foreach { z =>
      xRange.foreach { x =>
        yRange.foreach { y =>
          if (cube(z)(x)(y)) {
            active = active + 1
          }
        }
      }
    }
    active
  }

  def countActive4(cube: Array[Array[Array[Array[Boolean]]]], wRange: Range, zRange: Range, xRange: Range, yRange: Range): Int = {
    wRange.map { w =>
      countActive3(cube(w), zRange, xRange, yRange)
    }.sum
  }
}

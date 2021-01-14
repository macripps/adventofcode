package aoc2017

import aoc.Day

import scala.collection.mutable

class Day21 extends Day(2017, 21) {
  import Day21._

  val example = Seq(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#",
  )

  val start = Grid(Array(Array('.', '#', '.'), Array('.', '.', '#'), Array('#', '#', '#')))

  override def part1: String = result(5)

  override def part2: String = result(18)

  def result(its: Int): String = {
    val rules = mutable.Map[String, Grid]()
    var grid = start

    input.foreach { l =>
      val kv = l.split(" => ")
      val v = Grid(kv(1).split("/").map(x => x.toCharArray))
      val lines = kv(0).split("/").map(x => x.toCharArray)
      val grid = Grid(lines)

      rules.put(grid.singleLine, v)
      rules.put(grid.rotate.singleLine, v)
      rules.put(grid.rotate.rotate.singleLine, v)
      rules.put(grid.rotate.rotate.rotate.singleLine, v)
      rules.put(grid.flipH.singleLine, v)
      rules.put(grid.flipH.rotate.singleLine, v)
      rules.put(grid.flipV.singleLine, v)
      rules.put(grid.flipV.rotate.singleLine, v)
    }

    (1 to its).foreach { _ =>
      var newSize = 0
      var dlt = 0
      val split: Array[Array[Grid]] = if (grid.contents.length % 2 == 0) {
        newSize = (grid.contents.length * 3 / 2 )
        dlt = 3
        grid.splitAt(2)
      } else {
        newSize = (grid.contents.length * 4/ 3)
        dlt = 4
        grid.splitAt(3)
      }

      val result = split.map { ys =>
        ys.map { xs =>
          rules(xs.singleLine)
        }
      }

      val nc = Array.ofDim[Char](newSize, newSize)

      result.indices.foreach { y =>
        result(y).indices.foreach { x =>
          ((dlt * y) until (dlt * (y + 1))).foreach { k =>
            ((dlt * x) until (dlt * (x + 1))).foreach { l =>
              nc(k)(l) = result(y)(x).contents(k-(dlt * y))(l-(dlt * x))
            }
          }
        }
      }

      grid = Grid(nc)
    }

    grid.count.toString
  }
}

object Day21 {
  def apply() = new Day21()

  case class Grid(contents: Array[Array[Char]]) {

    def rotate: Grid = {
      val rotatedContents = Array.ofDim[Char](contents.length, contents(0).length)
      contents.indices.foreach { y =>
        contents(y).indices.foreach { x =>
          rotatedContents(x)(contents.length - 1 - y) = contents(y)(x)
        }
      }
      Grid(rotatedContents)
    }

    def flipH: Grid = {
      Grid(contents.map(_.reverse))
    }

    def flipV: Grid = {
      Grid(contents.reverse)
    }

    def asBytes = contents.map(_.map(_.toInt).mkString("(", ",", ")")).mkString("\n")
    def singleLine: String = contents.map(_.mkString).mkString
    override def toString = contents.map(_.mkString).mkString("\n")

    def count = contents.map(_.count(x => x == '#')).sum

    def splitAt(size: Int): Array[Array[Grid]] = {
      Range(0, contents.length, size).map { y =>
        Range(0, contents(y).length, size).map { x =>
          val g = Grid(contents.slice(y, y + size).map {
            _.slice(x, x + size)
          })
          g
        }.toArray
      }.toArray
    }
  }
}

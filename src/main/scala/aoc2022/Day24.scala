package aoc2022

import Day24._
import aoc.Maths

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day24 extends aoc.Day(2022, 24) {
  override def part1(input: Array[String]): Any = {
    val (blizzards, height, width) = parse(input)

    val versions = evolutions(blizzards, height, width)
    search(
      versions,
      height,
      width,
      (0, aoc.Point(1, 0)),
      aoc.Point(width - 2, height - 1),
    )
  }

  private[this] def search(
                            versions: Map[Int, Blizzards],
                            height: Int,
                            width: Int,
                            start: (Int, aoc.Point),
                            goal: aoc.Point
                          ): Int = {
    val (_, origin) = start
    val state = mutable.ArrayDeque[(Int, aoc.Point)](start)
    val seen = mutable.Set[(Int, aoc.Point)](start)
    var result = Int.MinValue
    breakable {
      while (state.nonEmpty) {
        val (time, parent) = state.removeHead()
        val t = (time + 1) % versions.size
        val bs = versions(t)
        (parent.neighbours :+ parent).foreach { child =>
          if (bs.exists { case (_, pts) => pts.contains(child) }) {
            // Ignore
          } else {
            if (child == goal) {
              result = time + 1
              break()
            }
            if (child != origin && (child.x <= 0 || child.x >= width - 1 || child.y <= 0 || child.y >= height - 1)) {
              // Ignore
            } else {
              if (seen.contains((t, child))) {
                // Ignore
              } else {
                seen.addOne((t, child))
                state.append((time + 1, child))
              }
            }
          }
        }
      }
    }
    result
  }

  private[this] def parse(input: Array[String]): (Blizzards, Int, Int) = {
    val blizzards = mutable.Map[aoc.Direction.Direction, mutable.Set[aoc.Point]]()
    input.indices.foreach { row =>
      input(row).indices.foreach { col =>
        val c = input(row)(col)
        val pos = aoc.Point(col, row)
        if (c == '>') {
          if (!blizzards.contains(aoc.Direction.East)) {
            blizzards(aoc.Direction.East) = mutable.Set[aoc.Point]()
          }
          blizzards(aoc.Direction.East).addOne(pos)
        } else if (c == '<') {
          if (!blizzards.contains(aoc.Direction.West)) {
            blizzards(aoc.Direction.West) = mutable.Set[aoc.Point]()
          }
          blizzards(aoc.Direction.West).addOne(pos)
        } else if (c == 'v') {
          if (!blizzards.contains(aoc.Direction.South)) {
            blizzards(aoc.Direction.South) = mutable.Set[aoc.Point]()
          }
          blizzards(aoc.Direction.South).addOne(pos)
        } else if (c == '^') {
          if (!blizzards.contains(aoc.Direction.North)) {
            blizzards(aoc.Direction.North) = mutable.Set[aoc.Point]()
          }
          blizzards(aoc.Direction.North).addOne(pos)
        }
      }
    }
    (blizzards.map { case (k, v) => k -> v.toSet }.toMap, input.length, input(0).length)
  }

  private[this] def evolutions(blizzards: Blizzards, height: Int, width: Int): Map[Int, Blizzards] = {
    val versions = mutable.Map[Int, Blizzards]()
    var bs = blizzards
    (0 until (((height - 2) * (width - 2)) / Maths.gcd(height - 2, width - 2))).foreach { i =>
      versions(i) = bs
      bs = evolve(bs, height, width)
    }
    versions.toMap
  }

  private[this] def evolve(blizzards: Blizzards, height: Int, width: Int): Blizzards = {
    val newBlizzards = mutable.Map[aoc.Direction.Direction, Set[aoc.Point]]()
    blizzards.foreach {
      case (direction, points) =>
        newBlizzards(direction) = points.map {
          blizzardLoc =>
            val nextPt = direction match {
              case aoc.Direction.North =>
                aoc.Point(blizzardLoc.x, blizzardLoc.y - 1)
              case aoc.Direction.South =>
                aoc.Point(blizzardLoc.x, blizzardLoc.y + 1)
              case aoc.Direction.East =>
                aoc.Point(blizzardLoc.x + 1, blizzardLoc.y)
              case aoc.Direction.West =>
                aoc.Point(blizzardLoc.x - 1, blizzardLoc.y)
            }
            val x = (width - 2 + nextPt.x - 1) % (width - 2)
            val y = (height - 2 + nextPt.y - 1) % (height - 2)
            aoc.Point(x + 1, y + 1)
        }
    }
    newBlizzards.toMap
  }

  val test =
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val (blizzards, height, width) = parse(input)

    val versions = evolutions(blizzards, height, width)
    val leg1 = search(
      versions,
      height,
      width,
      (0, aoc.Point(1, 0)),
      aoc.Point(width - 2, height - 1),
    )
    val leg2 = search(
      versions,
      height,
      width,
      (leg1, aoc.Point(width - 2, height - 1)),
      aoc.Point(1, 0),
    )
    search(
      versions,
      height,
      width,
      (leg2, aoc.Point(1, 0)),
      aoc.Point(width - 2, height - 1),
    )
  }
}

object Day24 {
  def apply() = new Day24

  type Blizzards = Map[aoc.Direction.Direction, Set[aoc.Point]]
}
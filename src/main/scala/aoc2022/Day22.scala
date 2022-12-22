package aoc2022

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day22 extends aoc.Day(2022, 22) {
  override def part1(input: Array[String]): Any = {
    val groups = inputGroups(input)
    val gridT = groups.head
    val instructions = groups.tail.head.head
    val grid = mutable.Map[aoc.Point, Char]()
    var row = 1
    gridT.foreach { line =>
      var col = 1
      line.foreach { char =>
        if (char != ' ') {
          grid(aoc.Point(col, row)) = char
        }
        col = col + 1
      }
      row = row + 1
    }
    var current = grid.filter(pt => pt._1.y == 1 && pt._2 == '.').minBy(pt => pt._1.x)._1
    var facing = 0 // Right
    var inst = 0
    while (inst < instructions.length) {
      if (instructions.charAt(inst) == 'L') {
        facing = (facing + 3) % 4
        inst = inst + 1
      } else if (instructions.charAt(inst) == 'R') {
        facing = (facing + 1) % 4
        inst = inst + 1
      } else {
        // Number
        val n = instructions.drop(inst).takeWhile(c => c >= '0' && c <= '9')
        val dist = n.toInt
        inst = inst + n.length
        breakable {
          (1 to dist).foreach { _ =>
            var next = if (facing == 0) {
              aoc.Point(current.x + 1, current.y)
            } else if (facing == 1) {
              aoc.Point(current.x, current.y + 1)
            } else if (facing == 2) {
              aoc.Point(current.x - 1, current.y)
            } else { // facing == 3
              aoc.Point(current.x, current.y - 1)
            }
            if (!grid.contains(next)) { // Wrap
              next = wrapOnTorus(grid.keys, next, facing)
            }
            if (grid(next) == '.') {
              current = next
            } else {
              // Must be a wall
              break()
            }
          }
        }
      }
    }
    (1000 * current.y) + (4 * current.x) + facing
  }

  val test =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val groups = inputGroups(input)
    val gridT = groups.head
    val instructions = groups.tail.head.head
    val grid = mutable.Map[aoc.Point, Char]()
    var row = 1
    gridT.foreach { line =>
      var col = 1
      line.foreach { char =>
        if (char != ' ') {
          grid(aoc.Point(col, row)) = char
        }
        col = col + 1
      }
      row = row + 1
    }
    var current = grid.filter(pt => pt._1.y == 1 && pt._2 == '.').minBy(pt => pt._1.x)._1
    var facing = 0 // Right
    var inst = 0
    while (inst < instructions.length) {
      println("(" + (current.y-1) + ", " + (current.x-1) + ") " + facing)
      if (instructions.charAt(inst) == 'L') {
        facing = (facing + 3) % 4
        inst = inst + 1
      } else if (instructions.charAt(inst) == 'R') {
        facing = (facing + 1) % 4
        inst = inst + 1
      } else {
        // Number
        val n = instructions.drop(inst).takeWhile(c => c >= '0' && c <= '9')
        val dist = n.toInt
        inst = inst + n.length
        breakable {
          (1 to dist).foreach { _ =>
            var next = if (facing == 0) {
              aoc.Point(current.x + 1, current.y)
            } else if (facing == 1) {
              aoc.Point(current.x, current.y + 1)
            } else if (facing == 2) {
              aoc.Point(current.x - 1, current.y)
            } else { // facing == 3
              aoc.Point(current.x, current.y - 1)
            }
            if (!grid.contains(next)) { // Wrap
              val nxt = wrapOnCube(grid.keys, next, facing)
              next = nxt._1
              facing = nxt._2
            }
            if (grid(next) == '.') {
              current = next
            } else {
              // Must be a wall
              break()
            }
          }
        }
      }
    }
    (1000 * current.y) + (4 * current.x) + facing
  }

  def wrapOnTorus(points: Iterable[aoc.Point], pt: aoc.Point, facing: Int): aoc.Point = {
    if (facing == 0) {
      points.filter(_.y == pt.y).minBy(_.x)
    } else if (facing == 1) {
      points.filter(_.x == pt.x).minBy(_.y)
    } else if (facing == 2) {
      points.filter(_.y == pt.y).maxBy(_.x)
    } else {
      points.filter(_.x == pt.x).maxBy(_.y)
    }
  }

  def wrapOnCube(points: Iterable[aoc.Point], pt: aoc.Point, facing: Int): (aoc.Point, Int) = {
    // Hard coded to input code
    if (pt.x == 50 && pt.y >= 1 && pt.y <= 50 && facing == 2) {
      // Warped off left side of top face
      (aoc.Point(1, 151 - pt.y), 0)
    } else if (pt.x >= 51 && pt.x <= 100 && pt.y == 0 && facing == 3) {
      // Warped off top side of top face
      (aoc.Point(1, pt.x + 100), 0)
    } else if (pt.x == 50 && pt.y >= 51 && pt.y <= 100 && facing == 2) {
      // Warped off left side of front face
      (aoc.Point(pt.y - 50,101), 1)
    } else if (pt.x == 101 && pt.y >= 51 && pt.y <= 100 && facing == 0) {
      // Warped off right side of front face
      (aoc.Point(pt.y + 50, 50), 3)
    } else if (pt.x >= 101 && pt.x <= 150 && pt.y == 0 && facing == 3) {
      // Warped off back of right face
      (aoc.Point(pt.x - 100, 200), 3)
    } else if (pt.x == 151 && pt.y>=1 && pt.y <= 50 && facing == 0) {
      // Warped off bottom of right face
      (aoc.Point(100, 151 - pt.y), 2)
    } else if (pt.x >= 101 && pt.x <= 150 && pt.y == 51 && facing == 1) {
      // Warped off front of right face
      (aoc.Point(100, pt.x - 50), 2)
    } else if (pt.x == 101 && pt.y >= 101 && pt.y <= 150 && facing == 0) {
      (aoc.Point(150, 151 - pt.y), 2)
    } else if (pt.x >= 51 && pt.x <= 100 && pt.y == 151 && facing == 1) {
      (aoc.Point(50, pt.x + 100), 2)
    } else if (pt.x >= 1 && pt.x <= 50 && pt.y == 100 && facing == 3) {
      (aoc.Point(51, pt.x + 50), 0)
    } else if (pt.x == 0 && pt.y >= 101 && pt.y <= 150 && facing == 2) {
      (aoc.Point(51, 151 - pt.y), 0)
    } else if (pt.x == 0 && pt.y >= 151 && pt.y <= 200 && facing == 2) {
      (aoc.Point(pt.y - 100, 1), 1)
    } else if (pt.x >= 1 && pt.x <= 50 && pt.y == 201 && facing == 1) {
      (aoc.Point(pt.x + 100, 1), 1)
    } else if (pt.x == 51 && pt.y >= 151 && pt.y <= 200 && facing == 0) {
      (aoc.Point(pt.y - 100, 150), 3)
    } else {
      println("Wrap " + (pt, facing) + " not handled")
      (pt, facing)
    }
  }
}

object Day22 {
  def apply() = new Day22
}
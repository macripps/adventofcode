package aoc2021

import aoc.{Day, Point}
import aoc2021.Day20.example

import scala.collection.mutable

class Day20 extends Day(2021, 20) {
  override def part1(input: Array[String]): String = {
    solution(input, 2).toString
  }

  def toBinary(string: String): Int = {
    Integer.parseInt(string.replace('.', '0').replace('#', '1'), 2)
  }

  def surroundings(pt: Point): List[Point] = {
    val x = pt.x
    val y = pt.y
    List(
      Point(x - 1, y - 1),
      Point(x, y - 1),
      Point(x + 1, y - 1),
      Point(x - 1, y),
      Point(x, y),
      Point(x + 1, y),
      Point(x - 1, y + 1),
      Point(x, y + 1),
      Point(x + 1, y + 1),
    )
  }

  def draw(m: mutable.Map[Point, Char]): Unit = {
    val minY = m.minBy(_._1.y)._1.y
    val maxY = m.maxBy(_._1.y)._1.y
    val minX = m.minBy(_._1.x)._1.x
    val maxX = m.maxBy(_._1.x)._1.x
    (minY - 5 to maxY + 5).foreach { y =>
      (minX - 5 to maxX + 5).foreach { x =>
        print(m(Point(x, y)))
      }
      println()
    }
  }

  override def part2(input: Array[String]): String = {
    solution(input, 50).toString
  }

  def solution(input: Array[String], times: Int): Int = {
    val enhancer = input.head.toCharArray
    val image = input.drop(2).map(_.toCharArray)
    var backgroundChar = '.'
    var grid = mutable.Map[Point, Char]().withDefaultValue(backgroundChar)
    image.indices.foreach { row =>
      image(row).indices.foreach { col =>
        grid(Point(col, row)) = image(row)(col)
      }
    }
    var minX = 0
    var minY = 0
    var maxX = image(0).length
    var maxY = image.length

    (1 to times).foreach { _ =>
      minX = minX - 1
      minY = minY - 1
      maxX = maxX + 1
      maxY = maxY + 1
      val backgroundCalc = new String(Array[Char](backgroundChar, backgroundChar, backgroundChar, backgroundChar, backgroundChar, backgroundChar, backgroundChar, backgroundChar, backgroundChar))
      val index = toBinary(backgroundCalc)
      backgroundChar = enhancer(index)
      val grid2 = mutable.Map[Point, Char]().withDefaultValue(backgroundChar)
      (minY to maxY).foreach { y =>
        (minX to maxX).foreach { x =>
          val pt = Point(x, y)
          val str = surroundings(pt).map(grid(_)).mkString
          grid2(pt) = enhancer(toBinary(str))
        }
      }
      grid = grid2
    }
    grid.count(_._2 == '#')
  }
}

object Day20 {
  def apply() = new Day20()

  val example =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###""".stripMargin.split("\n")
}

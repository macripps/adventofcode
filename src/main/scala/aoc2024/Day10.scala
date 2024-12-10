package aoc2024

import aoc.{Direction, Point}

import scala.collection.mutable

class Day10 extends aoc.NewDay(2024, 10) {

  part(1) {
    test(
      """...0...
        |...1...
        |...2...
        |6543456
        |7.....7
        |8.....8
        |9.....9""".stripMargin -> 2)
    test(
      """..90..9
        |...1.98
        |...2..7
        |6543456
        |765.987
        |876....
        |987....""".stripMargin -> 4)
    test(
      """10..9..
        |2...8..
        |3...7..
        |4567654
        |...8..3
        |...9..2
        |.....01""".stripMargin -> 3)
    test(
      """89010123
        |78121874
        |87430965
        |96549874
        |45678903
        |32019012
        |01329801
        |10456732""".stripMargin -> 36)

    execute { ls =>
      val grid = ls.map(_.toCharArray.map(_ - '0'))
      grid.indices.flatMap { y =>
        grid(y).indices.map { x =>
          if (grid(y)(x) == 0) {
            scoreFrom(grid, Point(x, y))
          } else 0
        }
      }.sum
    }

    def scoreFrom(grid: Array[Array[Int]], p: Point): Int = {
      val seen = mutable.Set[Point]()
      val points = mutable.Queue(p)
      while (points.nonEmpty) {
        val pt = points.dequeue()
        val v = grid(pt.y)(pt.x)
        if (v == 9 && !seen.contains(pt)) {
          seen += pt
        } else {
          List(
            Direction.North,
            Direction.East,
            Direction.South,
            Direction.West,
          ).foreach { d =>
            val n = pt.go(d)
            if (n.x >= 0 && n.y >= 0 && n.y < grid.length && n.x < grid(n.y).length && grid(n.y)(n.x) == v + 1) {
              points.enqueue(n)
            }
          }
        }
      }
      seen.size
    }
  }

  part(2) {
    test(
      """.....0.
        |..4321.
        |..5..2.
        |..6543.
        |..7..4.
        |..8765.
        |..9....""".stripMargin -> 3)
    test(
      """..90..9
        |...1.98
        |...2..7
        |6543456
        |765.987
        |876....
        |987....""".stripMargin -> 13)
    test(
      """012345
        |123456
        |234567
        |345678
        |4.6789
        |56789.""".stripMargin -> 227)
    test(
      """89010123
        |78121874
        |87430965
        |96549874
        |45678903
        |32019012
        |01329801
        |10456732""".stripMargin -> 81)
    execute { ls =>
      val grid = ls.map(_.toCharArray.map(_ - '0'))
      grid.indices.flatMap { y =>
        grid(y).indices.map { x =>
          if (grid(y)(x) == 0) {
            ratingFrom(grid, Point(x, y))
          } else 0
        }
      }.sum
    }

    def ratingFrom(grid: Array[Array[Int]], p: Point): Int = {
      var trails = 0
      val points = mutable.Queue(p)
      while (points.nonEmpty) {
        val pt = points.dequeue()
        val v = grid(pt.y)(pt.x)
        if (v == 9) {
          trails = trails + 1
        } else {
          List(
            Direction.North,
            Direction.East,
            Direction.South,
            Direction.West,
          ).foreach { d =>
            val n = pt.go(d)
            if (n.x >= 0 && n.y >= 0 && n.y < grid.length && n.x < grid(n.y).length && grid(n.y)(n.x) == v + 1) {
              points.enqueue(n)
            }
          }
        }
      }
      trails
    }
  }
}

object Day10Main extends Day10

package aoc2023

import aoc.Direction.Direction
import aoc.{Direction, NewDay, Point, Search}

class Day17 extends NewDay(2023, 17) {

  part(1) {
    test {
      """2413432311323
        |3215453535623
        |3255245654254
        |3446585845452
        |4546657867536
        |1438598798454
        |4457876987766
        |3637877979653
        |4654967986887
        |4564679986453
        |1224686865563
        |2546548887735
        |4322674655533""".stripMargin -> 102
    }

    execute { input =>
      val grid = input.map(_.map(_ - '0'))
      val start = Point(0, 0)
      val goal = Point(input(0).length - 1, input.length - 1)
      val route = Search.AStarWorking[(Point, Direction, Int)](
        (start, null, 0),
        pt => pt._1 == goal,
        pt => {
          List(Direction.North, Direction.East, Direction.South, Direction.West).filter { d =>
            pt._2 == null ||
              (d == Direction.North && pt._2 != Direction.South) ||
              (d == Direction.East && pt._2 != Direction.West) ||
              (d == Direction.South && pt._2 != Direction.North) ||
              (d == Direction.West && pt._2 != Direction.East)
          }.filter { d =>
            val n = pt._1.go(d)
            n.x >= 0 && n.x < grid(0).length && n.y >= 0 && n.y < grid.length
          }.map { d =>
            (pt._1.go(d), d, if (pt._2 == d) pt._3 + 1 else 1)
          }.filter { pts =>
            pts._3 < 4
          }
        },
        pt => grid(pt._1.y)(pt._1.x),
        pt => pt._1.manhattanDistanceTo(goal)
      )
      route.map(pt => grid(pt._1.y)(pt._1.x)).drop(1).sum
    }
  }

  part(2) {
    test {
      """2413432311323
        |3215453535623
        |3255245654254
        |3446585845452
        |4546657867536
        |1438598798454
        |4457876987766
        |3637877979653
        |4654967986887
        |4564679986453
        |1224686865563
        |2546548887735
        |4322674655533""".stripMargin -> 94
    }

    test {
      """111111111111
        |999999999991
        |999999999991
        |999999999991
        |999999999991""".stripMargin -> 71
    }

    execute { input =>
      val grid = input.map(_.map(_ - '0'))
      val start = Point(0, 0)
      val goal = Point(input(0).length - 1, input.length - 1)
      val route = Search.AStarWorking[(Point, Direction, Int)](
        (start, null, 0),
        pt => pt._1 == goal && pt._3 >= 4 && pt._3 <= 10,
        pt => {
          List(Direction.North, Direction.East, Direction.South, Direction.West).filter { d =>
            pt._2 == null ||
              (d == Direction.North && pt._2 != Direction.South) ||
              (d == Direction.East && pt._2 != Direction.West) ||
              (d == Direction.South && pt._2 != Direction.North) ||
              (d == Direction.West && pt._2 != Direction.East)
          }.filter { d =>
            val n = pt._1.go(d)
            n.x >= 0 && n.x < grid(0).length && n.y >= 0 && n.y < grid.length
          }.filter { d =>
            pt._2 == null || (d == pt._2 && pt._3 < 10) ||
              (d != pt._2 && pt._3 >= 4)
          }.map { d =>
            (pt._1.go(d), d, if (pt._2 == d) pt._3 + 1 else 1)
          }
        },
        pt => grid(pt._1.y)(pt._1.x),
        pt => pt._1.manhattanDistanceTo(goal)
      )
      route.map(pt => grid(pt._1.y)(pt._1.x)).drop(1).sum
    }
  }
}

object Day17Main extends Day17

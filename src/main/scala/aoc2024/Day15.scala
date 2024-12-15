package aoc2024

import aoc.Direction.North
import aoc.{Direction, NewDay, Point}

import scala.collection.mutable

class Day15 extends NewDay(2024, 15) {
  part(1) {
    test(
      """########
        |#..O.O.#
        |##@.O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |
        |<^^>>>vv<v>>v<<""".stripMargin -> 2028)
    test(
      """##########
        |#..O..O.O#
        |#......O.#
        |#.OO..O.O#
        |#..O@..O.#
        |#O#..O...#
        |#O..O..O.#
        |#.OO.O.OO#
        |#....O...#
        |##########
        |
        |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
        |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
        |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
        |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
        |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
        |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
        |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
        |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
        |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
        |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin -> 10092)
    execute { ls =>
      val is = aoc.asGroupsSeparatedByBlankLines(ls)
      val grid = is.head.map(_.toCharArray).toArray
      val insts = is.tail.head.mkString

      val start = grid.indices.zip(grid(0).indices).find { case (c, r) =>
        grid(c)(r) == '@'
      }.map { case (c, r) => Point(r, c) }.get

      var position = start
      insts.foreach { i =>
        val d = i match {
          case '<' => Direction.West
          case '>' => Direction.East
          case '^' => Direction.North
          case 'v' => Direction.South
        }
        var potSpace = position.go(d)
        var moves = 1
        while (potSpace.y >= 0 && potSpace.y < grid.length && potSpace.x >= 0 && potSpace.x < grid(potSpace.y).length && grid(potSpace.y)(potSpace.x) != '.' && grid(potSpace.y)(potSpace.x) != '#') {
          potSpace = potSpace.go(d)
          moves = moves + 1
        }
        if (potSpace.y >= 0 && potSpace.y < grid.length && potSpace.x >= 0 && potSpace.x < grid(potSpace.y).length && grid(potSpace.y)(potSpace.x) == '.') {
          (1 to moves).reverse.foreach { m =>
            val dest = position.go(d, m)
            val src = position.go(d, m - 1)
            grid(dest.y)(dest.x) = grid(src.y)(src.x)
          }
          grid(position.y)(position.x) = '.'
          position = position.go(d)
        }
      }

      var score = 0L
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          if (grid(c)(r) == 'O') {
            score = score + (100L * c) + r
          }
        }
      }
      score
    }
  }

  part(2) {
    test(
      """##########
        |#..O..O.O#
        |#......O.#
        |#.OO..O.O#
        |#..O@..O.#
        |#O#..O...#
        |#O..O..O.#
        |#.OO.O.OO#
        |#....O...#
        |##########
        |
        |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
        |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
        |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
        |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
        |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
        |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
        |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
        |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
        |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
        |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin -> 9021)
    execute { ls =>
      val is = aoc.asGroupsSeparatedByBlankLines(ls)
      val grid = is.head.map { s =>
        s.toCharArray.map {
          case '#' => "##"
          case 'O' => "[]"
          case '@' => "@."
          case '.' => ".."
        }.mkString.toCharArray
      }.toArray
      val insts = is.tail.head.mkString

      var start = Point(0, 0)
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          if (grid(c)(r) == '@') start = Point(r, c)
        }
      }

      var position = start
      insts.foreach { i =>
        val d = i match {
          case '<' => Direction.West
          case '>' => Direction.East
          case '^' => Direction.North
          case 'v' => Direction.South
        }
        d match {
          case Direction.East | Direction.West =>
            var potSpace = position.go(d)
            var moves = 1
            while (potSpace.y >= 0 && potSpace.y < grid.length && potSpace.x >= 0 && potSpace.x < grid(potSpace.y).length && grid(potSpace.y)(potSpace.x) != '.' && grid(potSpace.y)(potSpace.x) != '#') {
              potSpace = potSpace.go(d)
              moves = moves + 1
            }
            if (potSpace.y >= 0 && potSpace.y < grid.length && potSpace.x >= 0 && potSpace.x < grid(potSpace.y).length && grid(potSpace.y)(potSpace.x) == '.') {
              (1 to moves).reverse.foreach { m =>
                val dest = position.go(d, m)
                val src = position.go(d, m - 1)
                grid(dest.y)(dest.x) = grid(src.y)(src.x)
              }
              grid(position.y)(position.x) = '.'
              position = position.go(d)
            }
          case Direction.South | Direction.North =>
            val visited = mutable.Set[Point]()
            val pointsToMove = mutable.Queue[Point](position)
            var couldMove = true
            while (pointsToMove.nonEmpty && couldMove) {
              val pt = pointsToMove.dequeue()
              visited += pt
              val nextSpace = pt.go(d)
              grid(nextSpace.y)(nextSpace.x) match {
                case '[' =>
                  pointsToMove.enqueue(nextSpace)
                  pointsToMove.enqueue(nextSpace.go(Direction.East))
                case ']' =>
                  pointsToMove.enqueue(nextSpace)
                  pointsToMove.enqueue(nextSpace.go(Direction.West))
                case '#' => couldMove = false
                case '.' =>
              }
            }
            if (couldMove) {
              val pts =
                d match {
                  case Direction.South => visited.toList.sortBy(-_.y)
                  case Direction.North => visited.toList.sortBy(_.y)
                }
              pts.foreach { pt =>
                val dest = pt.go(d)
                grid(dest.y)(dest.x) = grid(pt.y)(pt.x)
                grid(pt.y)(pt.x) = '.'
              }
              grid(position.y)(position.x) = '.'
              position = position.go(d)
            }
        }
      }

      var score = 0L
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          if (grid(c)(r) == '[') {
            score = score + (100L * c) + r
          }
        }
      }
      score
    }
  }
}

object Day15Main extends Day15

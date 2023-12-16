package aoc2023

import aoc.{Direction, NewDay, Point}
import aoc2023.Day16Main.Beam

import scala.collection.mutable

class Day16 extends NewDay(2023, 16) {
  part(1) {
    test {
      """.|...\....
        ||.-.\.....
        |.....|-...
        |........|.
        |..........
        |.........\
        |..../.\\..
        |.-.-/..|..
        |.|....-|.\
        |..//.|....""".stripMargin -> 46
    }

    execute { input =>
      val grid = input.map(_.toCharArray)
      val start = Beam(Point(0, 0), Direction.East)

      energize(grid, start)
    }
  }

  private def energize(grid: Array[Array[Char]], start: Beam) = {
    val beams = mutable.Queue[Beam](start)
    val seen = mutable.Set[Beam]()
    while (beams.nonEmpty) {
      val beam = beams.dequeue()
      if (beam.position.x < 0 || beam.position.x >= grid(0).length || beam.position.y < 0 || beam.position.y >= grid.length) {
        // Edge of the grid
      } else {
        if (!seen.contains(beam)) {
          seen.add(beam)
          val entry = grid(beam.position.y)(beam.position.x)
          (entry, beam.direction) match {
            case ('.', _) => beams.append(beam.next)
            case ('\\', Direction.East) => beams.append(Beam(Point(beam.position.x, beam.position.y + 1), Direction.South))
            case ('\\', Direction.West) => beams.append(Beam(Point(beam.position.x, beam.position.y - 1), Direction.North))
            case ('\\', Direction.North) => beams.append(Beam(Point(beam.position.x - 1, beam.position.y), Direction.West))
            case ('\\', Direction.South) => beams.append(Beam(Point(beam.position.x + 1, beam.position.y), Direction.East))
            case ('/', Direction.East) => beams.append(Beam(Point(beam.position.x, beam.position.y - 1), Direction.North))
            case ('/', Direction.West) => beams.append(Beam(Point(beam.position.x, beam.position.y + 1), Direction.South))
            case ('/', Direction.North) => beams.append(Beam(Point(beam.position.x + 1, beam.position.y), Direction.East))
            case ('/', Direction.South) => beams.append(Beam(Point(beam.position.x - 1, beam.position.y), Direction.West))
            case ('|', Direction.North) => beams.append(Beam(Point(beam.position.x, beam.position.y - 1), Direction.North))
            case ('|', Direction.South) => beams.append(Beam(Point(beam.position.x, beam.position.y + 1), Direction.South))
            case ('|', _) => {
              beams.append(Beam(Point(beam.position.x, beam.position.y - 1), Direction.North))
              beams.append(Beam(Point(beam.position.x, beam.position.y + 1), Direction.South))
            }
            case ('-', Direction.East) => beams.append(Beam(Point(beam.position.x + 1, beam.position.y), Direction.East))
            case ('-', Direction.West) => beams.append(Beam(Point(beam.position.x - 1, beam.position.y), Direction.West))
            case ('-', _) => {
              beams.append(Beam(Point(beam.position.x - 1, beam.position.y), Direction.West))
              beams.append(Beam(Point(beam.position.x + 1, beam.position.y), Direction.East))
            }
          }
        }
      }
    }
    seen.map(_.position).size
  }

  part(2) {
    test {
      """.|...\....
        ||.-.\.....
        |.....|-...
        |........|.
        |..........
        |.........\
        |..../.\\..
        |.-.-/..|..
        |.|....-|.\
        |..//.|....""".stripMargin -> 51
    }

    execute { input =>
      val grid = input.map(_.toCharArray)
      (grid.indices.flatMap { y =>
        Set(Beam(Point(0, y), Direction.East), Beam(Point(grid(y).length - 1, y), Direction.West))
      } ++ grid(0).indices.flatMap { x =>
        Set(Beam(Point(x, 0), Direction.South), Beam(Point(x, grid.length - 1), Direction.North))
      }).map { start =>
        energize(grid, start)
      }.max
    }
  }
}

object Day16Main extends Day16 {
  case class Beam(position: Point, direction: Direction.Direction) {
    def next: Beam = Beam(position.go(direction), direction)
  }
}

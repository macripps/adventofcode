package aoc2023

import aoc.{Direction, Point}

import scala.collection.mutable

class Day14 extends aoc.NewDay(2023, 14) {

  part(1) {
    test {
      """O....#....
        |O.OO#....#
        |.....##...
        |OO.#O....O
        |.O.....O#.
        |O.#..O.#.#
        |..O..#O..O
        |.......O..
        |#....###..
        |#OO..#....""".stripMargin -> 136
    }

    execute { input =>
      val grid = input.map(_.toCharArray)
      val roundRocks = mutable.Buffer[Point]()
      val squareRocks = mutable.Buffer[Point]()
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (grid(row)(col) == 'O') {
            roundRocks.append(Point(col, row))
          } else if (grid(row)(col) == '#') {
            squareRocks.append(Point(col, row))
          }
        }
      }
      val direction = Direction.North

      val boundary = direction match {
        case Direction.North => pt: Point => pt.y > 0
        case Direction.East => pt: Point => pt.x < grid(0).length - 1
        case Direction.South => pt: Point => pt.y < grid.length - 1
        case Direction.West => pt: Point => pt.x > 0
      }

      val results = shift(roundRocks, boundary, squareRocks, direction)
      results.map { pt => (input.length - pt.y).toLong }.sum
    }
  }

  private[this] def shift(roundRocks: mutable.Buffer[Point], boundary: (Point => Boolean), squareRocks: mutable.Buffer[Point], direction: Direction.Direction) = {
    // Move north as far as possible
    val results = mutable.Buffer[Point]()
    var rocksToMove = direction match {
      case Direction.North => roundRocks.sortBy(_.y)
      case Direction.East => roundRocks.sortBy(-_.x)
      case Direction.South => roundRocks.sortBy(-_.y)
      case Direction.West => roundRocks.sortBy(_.x)
    }
    while (rocksToMove.nonEmpty) {
      val rock = rocksToMove.head
      rocksToMove = rocksToMove.tail
      var potential = rock
      while (boundary(potential) && !squareRocks.contains(potential.go(direction)) && !results.contains(potential.go(direction))) {
        potential = potential.go(direction)
      }
      results.append(potential)
    }
    results
  }

  part(2) {
    test {
      """O....#....
        |O.OO#....#
        |.....##...
        |OO.#O....O
        |.O.....O#.
        |O.#..O.#.#
        |..O..#O..O
        |.......O..
        |#....###..
        |#OO..#....""".stripMargin -> 64
    }

    execute { input =>
      val grid = input.map(_.toCharArray)
      var roundRocks = mutable.Buffer[Point]()
      val squareRocks = mutable.Buffer[Point]()
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (grid(row)(col) == 'O') {
            roundRocks.append(Point(col, row))
          } else if (grid(row)(col) == '#') {
            squareRocks.append(Point(col, row))
          }
        }
      }
      val maxCycles = 1000000000L
      var currentCycle = 0L

      val seen = mutable.Map[Set[Point], Long]()

      while (currentCycle < maxCycles) {
        if (debug()) println("Current cycle: " + currentCycle)
        currentCycle = currentCycle + 1
        roundRocks = spinCycle(grid(0).length, grid.length, roundRocks, squareRocks)

        val rockSet = roundRocks.toSet

        if (seen.contains(rockSet)) {
          if (debug()) println("Loop found - currentCycle " + currentCycle + ", lastSeen at " + seen(rockSet))
          val cycleLength = currentCycle  - seen(rockSet)
          currentCycle = currentCycle + ((maxCycles - currentCycle) / cycleLength) * cycleLength
        } else seen(rockSet) = currentCycle
      }

      roundRocks.map { pt => (input.length - pt.y).toLong }.sum
    }
  }

  val spinOrder = Array(Direction.North, Direction.West, Direction.South, Direction.East)

  private[this] def spinCycle(maxX: Int, maxY: Int, roundRocks: mutable.Buffer[Point], squareRocks: mutable.Buffer[Point]): mutable.Buffer[Point] = {
    var rocks = roundRocks
    spinOrder.foreach { direction =>
      val boundary = direction match {
        case Direction.North => pt: Point => pt.y > 0
        case Direction.East => pt: Point => pt.x < maxX - 1
        case Direction.South => pt: Point => pt.y < maxY - 1
        case Direction.West => pt: Point => pt.x > 0
      }
      rocks = shift(rocks, boundary, squareRocks, direction)
    }
    rocks
  }
}

object Day14Main extends Day14

package aoc2023

import scala.collection.mutable

class Day10 extends aoc.NewDay(2023, 10) {

  import aoc.Point

  part(1) {
    test {
      """.....
        |.S-7.
        |.|.|.
        |.L-J.
        |.....""".stripMargin -> 4
    }
    test {
      """..F7.
        |.FJ|.
        |SJ.L7
        ||F--J
        |LJ...""".stripMargin -> 8
    }
    execute { input =>
      val grid = input.map { rows => rows.toCharArray }
      val startRow = grid.indices.find { r => grid(r).indexOf('S') >= 0 }.get
      val startCol = grid(startRow).indexOf('S')
      val start = Point(startCol, startRow)
      val neighbours = start.neighbours.filter { pt =>
        pt.y >= 0 && pt.y < grid.length && pt.x >= 0 && pt.x <= grid(0).length
      }
      val connectedToLeft = neighbours.contains(Point(start.x - 1, start.y)) && {
        val c = grid(start.y)(start.x - 1)
        c == '-' || c == 'F' || c == 'L'
      }
      val connectedToRight = neighbours.contains(Point(start.x + 1, start.y)) && {
        val c = grid(start.y)(start.x + 1)
        c == '-' || c == 'J' || c == '7'
      }
      val connectedToUp = neighbours.contains(Point(start.x, start.y - 1)) && {
        val c = grid(start.y - 1)(start.x)
        c == '|' || c == 'F' || c == '7'
      }
      val connectedToDown = neighbours.contains(Point(start.x, start.y + 1)) && {
        val c = grid(start.y + 1)(start.x)
        c == '|' || c == 'J' || c == 'L'
      }
      val (replaceMe, from) = (connectedToUp, connectedToDown, connectedToLeft, connectedToRight) match {
        case (false, false, true, true) => ('-', Point(start.x - 1, start.y))
        case (false, true, false, true) => ('F', Point(start.x + 1, start.y))
        case (false, true, true, false) => ('7', Point(start.x - 1, start.y))
        case (true, false, false, true) => ('L', Point(start.x + 1, start.y))
        case (true, false, true, false) => ('J', Point(start.x - 1, start.y))
        case (true, true, false, false) => ('|', Point(start.x, start.y - 1))
        case _ => throw new IllegalArgumentException("Not connected to two pipes")
      }
      var current = start
      grid(start.y)(start.x) = replaceMe
      var last = from
      var distance = 0
      do {
        val next = grid(current.y)(current.x) match {
          case '-' => Set(Point(current.x - 1, current.y), Point(current.x + 1, current.y))
          case '7' => Set(Point(current.x - 1, current.y), Point(current.x, current.y + 1))
          case 'J' => Set(Point(current.x - 1, current.y), Point(current.x, current.y - 1))
          case 'F' => Set(Point(current.x + 1, current.y), Point(current.x, current.y + 1))
          case 'L' => Set(Point(current.x + 1, current.y), Point(current.x, current.y - 1))
          case '|' => Set(Point(current.x, current.y - 1), Point(current.x, current.y + 1))
        }
        val nextC = (next - last).head
        last = current
        current = nextC
        distance = distance + 1
      } while (current != start)
      distance / 2
    }
  }

  part(2) {
    test {
      """...........
        |.S-------7.
        |.|F-----7|.
        |.||.....||.
        |.||.....||.
        |.|L-7.F-J|.
        |.|..|.|..|.
        |.L--J.L--J.
        |...........""".stripMargin -> 4
    }
    test {
      """.F----7F7F7F7F-7....
        |.|F--7||||||||FJ....
        |.||.FJ||||||||L7....
        |FJL7L7LJLJ||LJ.L-7..
        |L--J.L7...LJS7F-7L7.
        |....F-J..F7FJ|L7L7L7
        |....L7.F7||L7|.L7L7|
        |.....|FJLJ|FJ|F7|.LJ
        |....FJL-7.||.||||...
        |....L---J.LJ.LJLJ...""".stripMargin -> 8
    }
    test {
      """FF7FSF7F7F7F7F7F---7
        |L|LJ||||||||||||F--J
        |FL-7LJLJ||||||LJL-77
        |F--JF--7||LJLJ7F7FJ-
        |L---JF-JLJ.||-FJLJJ7
        ||F|F-JF---7F7-L7L|7|
        ||FFJF7L7F-JF7|JL---7
        |7-L-JL7||F7|L7F-7F7|
        |L.L7LFJ|||||FJL7||LJ
        |L7JLJL-JLJLJL--JLJ.L""".stripMargin -> 10
    }
    execute { input =>
      val grid = input.map { rows => rows.toCharArray }
      val startRow = grid.indices.find { r => grid(r).indexOf('S') >= 0 }.get
      val startCol = grid(startRow).indexOf('S')
      val start = Point(startCol, startRow)
      val neighbours = start.neighbours.filter { pt =>
        pt.y >= 0 && pt.y < grid.length && pt.x >= 0 && pt.x <= grid(0).length
      }
      val connectedToLeft = neighbours.contains(Point(start.x - 1, start.y)) && {
        val c = grid(start.y)(start.x - 1)
        c == '-' || c == 'F' || c == 'L'
      }
      val connectedToRight = neighbours.contains(Point(start.x + 1, start.y)) && {
        val c = grid(start.y)(start.x + 1)
        c == '-' || c == 'J' || c == '7'
      }
      val connectedToUp = neighbours.contains(Point(start.x, start.y - 1)) && {
        val c = grid(start.y - 1)(start.x)
        c == '|' || c == 'F' || c == '7'
      }
      val connectedToDown = neighbours.contains(Point(start.x, start.y + 1)) && {
        val c = grid(start.y + 1)(start.x)
        c == '|' || c == 'J' || c == 'L'
      }
      val (replaceMe, from) = (connectedToUp, connectedToDown, connectedToLeft, connectedToRight) match {
        case (false, false, true, true) => ('-', Point(start.x - 1, start.y))
        case (false, true, false, true) => ('F', Point(start.x + 1, start.y))
        case (false, true, true, false) => ('7', Point(start.x - 1, start.y))
        case (true, false, false, true) => ('L', Point(start.x + 1, start.y))
        case (true, false, true, false) => ('J', Point(start.x - 1, start.y))
        case (true, true, false, false) => ('|', Point(start.x, start.y - 1))
        case _ => throw new IllegalArgumentException("Not connected to two pipes")
      }
      var current = start
      grid(start.y)(start.x) = replaceMe
      var last = from
      val mainLoop = mutable.Set[Point](start)
      do {
        val next = grid(current.y)(current.x) match {
          case '-' => Set(Point(current.x - 1, current.y), Point(current.x + 1, current.y))
          case '7' => Set(Point(current.x - 1, current.y), Point(current.x, current.y + 1))
          case 'J' => Set(Point(current.x - 1, current.y), Point(current.x, current.y - 1))
          case 'F' => Set(Point(current.x + 1, current.y), Point(current.x, current.y + 1))
          case 'L' => Set(Point(current.x + 1, current.y), Point(current.x, current.y - 1))
          case '|' => Set(Point(current.x, current.y - 1), Point(current.x, current.y + 1))
        }
        val nextC = (next - last).head
        last = current
        current = nextC
        mainLoop += current
      } while (current != start)
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (!mainLoop.contains(Point(col, row))) {
            grid(row)(col) = '.'
          }
        }
      }
      var insideLoop = 0
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (grid(row)(col) == '.') {
            var inRow = grid(row).take(col).mkString("")
            inRow = inRow.replaceAll("F-*J", "|")
            inRow = inRow.replaceAll("L-*7", "|")
            inRow = inRow.replaceAll("F-*7", "")
            inRow = inRow.replaceAll("L-*J", "")
            if (inRow.count(c => c != '.' && c != '-') % 2 != 0) {
              insideLoop = insideLoop + 1
            }
          }
        }
      }
      insideLoop
    }
  }
}

object Day10Main extends Day10

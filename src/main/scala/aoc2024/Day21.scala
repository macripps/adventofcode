package aoc2024;

import aoc.Direction.North
import aoc.{Direction, NewDay, Point, Search};

class Day21 extends NewDay(2024, 21) {
  part(1) {
    test(
      """029A
        |980A
        |179A
        |456A
        |379A""".stripMargin -> 126384L)
    execute { ls =>
      val keypad = Array(
        Array('7', '8', '9'),
        Array('4', '5', '6'),
        Array('1', '2', '3'),
        Array('x', '0', 'A'),
      )
      val directionPad = Array(
        Array('x', '^', 'A'),
        Array('<', 'v', '>'),
      )
      ls.map { l =>
        var robot1Instructions = ""
        var robot2Instructions = ""
        var meInstructions = ""
        var arm = Point(2, 3)
        var robot1 = Point(2, 0)
        var robot2 = Point(2, 0)
        l.foreach { c =>
          println(colors(5) + "[Robot1]" + reset + " Attempting to press " + c)
          val goal = findInGrid(keypad, c)
          val route = search(keypad, arm, goal)
          arm = goal
          println(colors(5) + "[Robot1]" + reset + " Shortest route (search): " + route)
          robot1Instructions = robot1Instructions + route
          println(colors(5) + "[Robot1]" + reset + " DirectionPad instructions required: " + route)
          route.foreach { d1 =>
            println(colors(3) + "[Robot2]" + reset + " Attempting to press " + d1)
            val goal2 = findInGrid(directionPad, d1)
            val robot2Directions = search(directionPad, robot1, goal2)
            robot1 = goal2
            println(colors(3) + "[Robot2]" + reset + " DirectionPad instructions required: " + robot2Directions)
            robot2Instructions = robot2Instructions + robot2Directions
            robot2Directions.foreach { d2 =>
              println(colors(9) + "[Me]" + reset + " Attempting to press " + d2)
              val goal3 = findInGrid(directionPad, d2)
              val meDirections = search(directionPad, robot2, goal3)
              robot2 = goal3
              println(colors(9) + "[Me]" + reset + " DirectionPad instructions required: " + meDirections)
              meInstructions = meInstructions + meDirections
            }
          }
        }
        println(meInstructions)
        println(robot2Instructions)
        println(robot1Instructions)
        println(l)
        print(meInstructions.length, l.filter(c => c.isDigit).dropWhile(c => c == '0').toInt)
        meInstructions.length * l.filter(c => c.isDigit).dropWhile(c => c == '0').toInt
      }.sum
    }
  }

  private[this] def betterNeighbours(p: Point): List[Point] = List(
    p.go(Direction.West),
    p.go(Direction.South),
    p.go(Direction.North),
    p.go(Direction.East)
  )

  private[this] def search(grid: Array[Array[Char]], start: Point, goal: Point): String = {
    val shortestPath = Search.AStarWorking[Point](start, p => p == goal, p => betterNeighbours(p).filter {
      n => n.y >= 0 && n.y < grid.length && n.x >= 0 && n.x < grid(n.y).length && grid(n.y)(n.x) != 'x'
    }, _ => 1, p => p.manhattanDistanceTo(goal))
    var at = start
    val directions = shortestPath.drop(1).map { n =>
      val dir = at.directionTo(n)
      at = n
      dir match {
        case Direction.North => '^'
        case Direction.East => '>'
        case Direction.South => 'v'
        case Direction.West => '<'
      }
    }
    directions.mkString.sorted.reverse + "A"
  }

  private[this] val colors = Array(
    "\u001B[38;2;0;0;0m", // 0 -> Black
    "\u001B[38;2;43;35;41m", // 1 ->
    "\u001B[38;2;66;57;85m", // 2 ->
    "\u001B[38;2;62;66;106m", // 3 ->
    "\u001B[38;2;64;96;128m", // 4 ->
    "\u001B[38;2;62;142;149m", // 5 ->
    "\u001B[38;2;57;170;132m", // 6 ->
    "\u001B[38;2;48;191;84m", // 7 ->
    "\u001B[38;2;65;212;35m", // 8 ->
    "\u001B[38;2;255;255;0m", // 9 -> Yellow
    "\u001B[38;2;15m", // 10 -> White
  )
  private[this] val reset = "\u001B[0m"


  private[this] def findInGrid(grid: Array[Array[Char]], ch: Char): Point = {
    grid.indices.find { c =>
      grid(c).indices.exists { r => grid(c)(r) == ch }
    }.flatMap { c =>
      grid(c).indices.find { r => grid(c)(r) == ch }.map { r =>
        Point(r, c)
      }
    }.get
  }
}

object Day21Main extends Day21

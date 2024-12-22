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
        var arm = Point(2, 3)
        var routesAvailable = List[String]("")
        var shortestRoute = Long.MaxValue
        println(colors(5) + "[Robot1]" + reset + " Goal: " + l)
        l.foreach { c =>
          //          println(colors(5) + "[Robot1]" + reset + " Attempting to press " + c)
          val goal = findInGrid(keypad, c)
          val routes = allPaths(keypad, arm, goal, "")
          routesAvailable = routesAvailable.flatMap { rs =>
            routes.map { r =>
              rs + r
            }
          }
          arm = goal
        }
        println(colors(5) + "[Robot1]" + reset + " Shortest routes available: " + routesAvailable.filter(_.length == routesAvailable.map(_.length).min))
        routesAvailable.foreach { route =>
          var robot1 = Point(2, 0)
          var nextRoutes = List("")
          route.foreach { c =>
            //            println(colors(3) + "[Robot2]" + reset + " Attempting to press " + c)
            val goal = findInGrid(directionPad, c)
            val routes = allPaths(directionPad, robot1, goal, "")
            nextRoutes = nextRoutes.flatMap { rs =>
              routes.map { r =>
                rs + r
              }
            }
            robot1 = goal
          }
          println(colors(3) + "[Robot2]" + reset + " Shortest route available: " + nextRoutes.filter(_.length == nextRoutes.map(_.length).min))
          nextRoutes.foreach { route =>
            var robot2 = Point(2, 0)
            var myRoutes = List("")
            route.foreach { c =>
              //              println(colors(9) + "[Me]" + reset + " Attempting to press " + c)
              val goal = findInGrid(directionPad, c)
              val routes = allPaths(directionPad, robot2, goal, "")
              myRoutes = myRoutes.flatMap { rs =>
                routes.map { r =>
                  rs + r
                }
              }.filter(_.length < shortestRoute)
              robot2 = goal
            }
            if (myRoutes.nonEmpty) {
              val myShortestRoute = myRoutes.filter(_.length == myRoutes.map(_.length).min).head
              println(colors(3) + "[Me]" + reset + "Shortest route available: " + myShortestRoute)
              if (myShortestRoute.length < shortestRoute) {
                shortestRoute = myShortestRoute.length
              }
            }
          }

        }
        shortestRoute * l.filter(_.isDigit).dropWhile(_ == '0').toInt
      }.sum
    }
  }


  private[this] def allPaths(array: Array[Array[Char]], point: aoc.Point, goal: aoc.Point, soFar: String): List[String] = {
    if (point == goal) {
      List(soFar + "A")
    } else if (array(point.y)(point.x) == 'x') {
      List()
    } else {
      (if (point.x > goal.x) {
        allPaths(array, point.go(Direction.West), goal, soFar + "<")
      } else if (point.x < goal.x) {
        allPaths(array, point.go(Direction.East), goal, soFar + ">")
      } else List()) ++ (
        if (point.y < goal.y) {
          allPaths(array, point.go(Direction.South), goal, soFar + "v")
        } else if (point.y > goal.y) {
          allPaths(array, point.go(Direction.North), goal, soFar + "^")
        } else List()
        )
    }
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

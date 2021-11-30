package aoc2016

import aoc.{Day, Point, Search}

class Day24 extends Day(2016, 24) {
  override def part1(input: Array[String]): String = {
    val grid = input.map(_.toCharArray)
    val locs = Map.newBuilder[Int, Point]
    grid.indices.foreach { y =>
      grid(y).indices.foreach { x =>
        if (grid(y)(x) >= '0' && grid(y)(x) <= '7') {
          locs.addOne(grid(y)(x) - '0', Point(x, y))
        }
      }
    }
    val locations = locs.result()
    val ptDistances = Map.newBuilder[(Int, Int), Int]
    locations.keys.toList.combinations(2).foreach { xs =>
      val start = locations(xs.head)
      val goal = locations(xs(1))
      val path = Search.AStar[Point](start, goal, x => x.neighbours.filter { p => grid(p.y)(p.x) != '#' }, x => x.manhattanDistanceTo(goal))
      ptDistances.addOne((xs.head, xs(1)), path.length - 1)
      ptDistances.addOne((xs(1), xs.head), path.length - 1)
    }
    val distances = ptDistances.result()
    var minDistance = Int.MaxValue
    (1 to locations.keys.max).permutations.foreach { order =>
      var thisDistance = 0
      val route = 0 +: order
      route.indices.dropRight(1).foreach { i =>
        thisDistance += distances(route(i), route(i+1))
      }
      if (thisDistance < minDistance) {
        println(route.toString() + ": " + thisDistance)
        minDistance = thisDistance
      }
    }
    minDistance.toString
  }

  def allSurroundings(x: Int, y: Int, grid: Array[Array[Char]]): Int = Seq((x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)).map(c => grid(c._2)(c._1)).count(_ == '#')

  override def part2(input: Array[String]): String = {
    val grid = input.map(_.toCharArray)
    val locs = Map.newBuilder[Int, Point]
    grid.indices.foreach { y =>
      grid(y).indices.foreach { x =>
        if (grid(y)(x) >= '0' && grid(y)(x) <= '7') {
          locs.addOne(grid(y)(x) - '0', Point(x, y))
        }
      }
    }
    val locations = locs.result()
    val ptDistances = Map.newBuilder[(Int, Int), Int]
    locations.keys.toList.combinations(2).foreach { xs =>
      val start = locations(xs.head)
      val goal = locations(xs(1))
      val path = Search.AStar[Point](start, goal, x => x.neighbours.filter { p => grid(p.y)(p.x) != '#' }, x => x.manhattanDistanceTo(goal))
      ptDistances.addOne((xs.head, xs(1)), path.length - 1)
      ptDistances.addOne((xs(1), xs.head), path.length - 1)
    }
    val distances = ptDistances.result()
    var minDistance = Int.MaxValue
    (1 to locations.keys.max).permutations.foreach { order =>
      var thisDistance = 0
      val route = 0 +: order :+ 0
      route.indices.dropRight(1).foreach { i =>
        thisDistance += distances(route(i), route(i+1))
      }
      if (thisDistance < minDistance) {
        println(route.toString() + ": " + thisDistance)
        minDistance = thisDistance
      }
    }
    minDistance.toString
  }
}

object Day24 {
  def apply() = new Day24()
}

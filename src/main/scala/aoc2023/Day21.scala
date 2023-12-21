package aoc2023

import aoc.{NewDay, Point}

import scala.collection.mutable

class Day21 extends NewDay(2023, 21) {

  part(1) {
    execute { input =>
      val grid = input.map(_.toCharArray)
      var start: Point = null
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (grid(row)(col) == 'S') {
            start = Point(col, row)
          }
        }
      }
      var pts = Set(start)
      (1 to 64).foreach { i =>
        pts = pts.flatMap { pt =>
          pt.neighbours.filter { p =>
            grid(p.y)(p.x) != '#'
          }
        }
      }
      pts.size
    }
  }

  // Thanks to OilAppropriate2827
  part(2) {
    execute { input =>
      val grid = input.map(_.toCharArray)
      val n = input.length
      var start: Point = null
      var pts = Set(start)
      val sparse = mutable.Set[Point]()
      grid.indices.foreach { row =>
        grid(row).indices.foreach { col =>
          if (grid(row)(col) != '#') {
            sparse.add(Point(col, row))
            if (grid(row)(col) == 'S') {
              start = Point(col, row)
            }
          }
        }
      }
      var visited = mutable.Set(start)
      var news = mutable.Set(start)
      val cache = mutable.Map(0L -> 1)
      val k = 26501365L / n
      val r = 26501365L % n

      def modp(point: Point): Point = {
        var y = point.y
        while (y < 0) {
          y = y + n
        }
        y = y % grid.length
        var x = point.x
        while (x < 0) {
          x = x + n
        }
        Point(x % n, y % n)
      }

      (1L to r + 2 * n + 1).foreach { c =>
        val old = news
        news = news.flatMap(_.neighbours.filter { np => !visited.contains(np) && sparse.contains(modp(np)) })
        visited = old
        cache(c) = news.size + (if (c > 1) cache(c - 2) else 0)

      }

      val d2 = cache(r+2*n)+cache(r)-2*cache(r+n)
      val d1 = cache(r+2*n)-cache(r+n)
      cache(r+2*n)+(k-2)*(2*d1+(k-1)*d2)/2
    }
  }
}

object Day21Main extends Day21

package aoc2024

import aoc.Direction.{East, North, South, West}
import aoc.Point

import scala.collection.mutable

class Day12 extends aoc.NewDay(2024, 12) {
  part(1) {
    test("""AAAA
           |BBCD
           |BBCC
           |EEEC""".stripMargin -> 140)
    test("""OOOOO
           |OXOXO
           |OOOOO
           |OXOXO
           |OOOOO""".stripMargin -> 772)
    test("""RRRRIICCFF
           |RRRRIICCCF
           |VVRRRCCFFF
           |VVRCCCJFFF
           |VVVVCJJCFE
           |VVIVCCJJEE
           |VVIIICJJEE
           |MIIIIIJJEE
           |MIIISIJEEE
           |MMMISSJEEE""".stripMargin -> 1930)
    execute { ls =>
      val grid = ls.map(_.toCharArray)
      var result = 0
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          val ch = grid(c)(r)
          val pt = Point(r,c)
          if (ch != '.') {
            val region = mutable.Set[Point]()
            val toVisit = mutable.Queue[Point](pt)
            while (toVisit.nonEmpty) {
              val nxt = toVisit.dequeue()
              region += nxt
              grid(nxt.y)(nxt.x) = '.'
              nxt.neighbours.foreach { nx =>
                if (nx.y >= 0 && nx.y < grid.length && nx.x >=0 && nx.x < grid(nx.y).length && grid(nx.y)(nx.x) == ch && !toVisit.contains(nx)) {
                  toVisit.enqueue(nx)
                }
              }
            }
            val perim = region.toList.map { pt =>
              4 - region.intersect(pt.neighbours.toSet).size
            }.sum
            result = result + region.size * perim
          }
        }
      }
      result
    }
  }

  part(2) {
    test("""AAAA
           |BBCD
           |BBCC
           |EEEC""".stripMargin -> 80)
    test("""OOOOO
           |OXOXO
           |OOOOO
           |OXOXO
           |OOOOO""".stripMargin -> 436)
    test("""EEEEE
           |EXXXX
           |EEEEE
           |EXXXX
           |EEEEE""".stripMargin -> 236)
    test("""AAAAAA
           |AAABBA
           |AAABBA
           |ABBAAA
           |ABBAAA
           |AAAAAA""".stripMargin -> 368)
    test("""RRRRIICCFF
           |RRRRIICCCF
           |VVRRRCCFFF
           |VVRCCCJFFF
           |VVVVCJJCFE
           |VVIVCCJJEE
           |VVIIICJJEE
           |MIIIIIJJEE
           |MIIISIJEEE
           |MMMISSJEEE""".stripMargin -> 1206)
    execute { ls =>
      val grid = ls.map(_.toCharArray)
      var result = 0
      grid.indices.foreach { c =>
        grid(c).indices.foreach { r =>
          val ch = grid(c)(r)
          val pt = Point(r, c)
          if (ch != '.') {
            val region = mutable.Set[Point]()
            val toVisit = mutable.Queue[Point](pt)
            while (toVisit.nonEmpty) {
              val nxt = toVisit.dequeue()
              region += nxt
              grid(nxt.y)(nxt.x) = '.'
              nxt.neighbours.foreach { nx =>
                if (nx.y >= 0 && nx.y < grid.length && nx.x >= 0 && nx.x < grid(nx.y).length && grid(nx.y)(nx.x) == ch && !toVisit.contains(nx)) {
                  toVisit.enqueue(nx)
                }
              }
            }
            val edges = region.flatMap { pt =>
              val x = mutable.Set[Edge]()
              if (!region.contains(pt.go(North))) {
                x += Edge(pt, pt.go(East))
              }
              if (!region.contains(pt.go(South))) {
                x += Edge(Point(pt.x, pt.y+1), Point(pt.x+1, pt.y+1))
              }
              if (!region.contains(pt.go(East))) {
                x += Edge(Point(pt.x+1, pt.y), Point(pt.x+1, pt.y+1))
              }
              if (!region.contains(pt.go(West))) {
                x += Edge(Point(pt.x, pt.y), Point(pt.x, pt.y+1))
              }
              x
            }
            var collapsed = true
            while (collapsed) {
              collapsed = false
              val canCollapse = edges.find { e1 =>
                edges.exists { e2 =>
                  e1.p2 == e2.p1 && e1.p1.directionTo(e1.p2) == e2.p1.directionTo(e2.p2)
                } && !edges.exists { e2 =>
                  e1.p2 == e2.p1 && e1.p1.directionTo(e1.p2) != e2.p1.directionTo(e2.p2)
                }
              }
              canCollapse.foreach { e1 =>
                val e2 = edges.find { e2: Edge => e1.p2 == e2.p1 && e1.p1.directionTo(e1.p2) == e2.p1.directionTo(e2.p2) }.get
                edges -= e1
                edges -= e2
                edges += Edge(e1.p1, e2.p2)
                collapsed = true
              }
            }
            result = result + (region.size * edges.size)
          }
        }
      }
      result
    }
    case class Edge(p1: Point, p2: Point)
  }
}

object Day12Main extends Day12

package aoc2016

import aoc.{NewDay, Search}

class Day13 extends NewDay(2016, 13) {
  import Day13._

  part(1) {
    execute { in =>
      val faveNum = in(0).toInt
      var result = Search.breadthFirst(Point(1,1)(None), nextNodes(faveNum), (p: Point) => p.x == 31 && p.y == 39)
      var length = 0
      while (result.from.isDefined) {
        length = length + 1
        result = result.from.get
      }
      length.toString
    }
  }

  def nextNodes(fav: Int)(pt: Point): Set[Point] = {
    pt.neighbours.filter {
      isSpace(fav)
    }
  }

  def isSpace(fav: Int)(p: Point): Boolean = {
    val z = (p.x * p.x + 3 * p.x + 2 * p.x * p.y + p.y + p.y * p.y) + fav
    Integer.bitCount(z) % 2 == 0
  }

  part(2) {
    execute { in =>
      val faveNum = in(0).toInt
      val nodes = Set.newBuilder[Point]
      nodes.addOne(Point(1,1)(None))
      var ns = Set(Point(1,1)(None))
      (1 to 50).foreach { s =>
        val out = ns.flatMap(nextNodes(faveNum))
        nodes.addAll(out)
        ns = out
      }
      nodes.result().size.toString
    }
  }
}

object Day13 {
  case class Point(x: Int, y: Int)(val from: Option[Point]) {

    def neighbours: Set[Point] = {
      val nextPoints = Set.newBuilder[Point]
      if (x > 0) {
        nextPoints.addOne(Point(x - 1, y)(Some(this)))
      }
      if (y > 0) {
        nextPoints.addOne(Point(x, y - 1)(Some(this)))
      }
      nextPoints.addOne(Point(x + 1, y)(Some(this)))
      nextPoints.addOne(Point(x, y + 1)(Some(this)))

      nextPoints.result()
    }
  }
}

object Day13Main extends Day13

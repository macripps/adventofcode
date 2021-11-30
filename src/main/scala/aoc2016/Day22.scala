package aoc2016

import aoc.Day

import scala.util.matching.Regex

class Day22 extends Day(2016, 22) {

  import Day22._

  def nodes(input: Array[String]): Array[Node] = input.drop(2).map {
    case line(x: String, y: String, size: String, used: String) => Node("node-x" + x + "-y" + y, x.toInt, y.toInt, size.toInt, used.toInt)
  }

  override def part1(input: Array[String]): String = {
    val nods = nodes(input)
    nods.filter(_.used != 0).flatMap { n =>
      nods.filter { n2 => n.name != n2.name && n2.avail >= n.used }
    }.length.toString
  }

  override def part2(input: Array[String]): String = {
    val m = nodes(input).map { n => Point(n.x, n.y) -> n }.toMap
    val maxX = m.keys.maxBy(_.x).x
    val maxY = m.keys.maxBy(_.y).y
    val g = Point(maxX, 0)
    (0 to maxY).foreach { y =>
      (0 to maxX).foreach { x =>
        val n = m(Point(x, y))
        if (x == 0 && y == 0) {
          print('(')
        } else {
          print(' ')
        }
        if (n.x == g.x && n.y == g.y) {
          print('G')
        } else if (n.used == 0) {
          print('_')
        } else if (n.size > 100) {
          print('#')
        } else {
          print('.')
        }
        if (x == 0 && y == 0) {
          print(')')
        } else {
          print(' ')
        }
      }
      println()
    }
    // Number of moves to get to (maxX-1, 0) -> 26
    // Number of moves to get to (maxX, 0) -> 1 (goal is now at maxX-1, 0)
    // Number of moves to shift goal left 1 -> 5
    // Number of times to shift goal left -> maxX-1

    (26 + 1 + (5 * 33)).toString
  }

  def nextStates(s: State): Set[State] = {
    val o = Set.newBuilder[State]
    val m = s.nodes.map { n => Point(n.x, n.y) -> n }.toMap
    val maxX = m.keys.maxBy(_.x).x
    val maxY = m.keys.maxBy(_.y).y
    s.nodes.filter(_.used != 0).foreach { n =>
      if (n.x > 0) {
        val leftNode = m(Point(n.x - 1, n.y))
        if (leftNode.avail >= n.used) {
          val t = s.nodes - n - leftNode
          val nextG = if (s.goalData.x == n.x && s.goalData.y == n.y) {
            Point(n.x - 1, n.y)
          } else s.goalData
          o.addOne(State(t + Node(n.name, n.x, n.y, n.size, 0) + Node(leftNode.name, n.x - 1, n.y, leftNode.size, leftNode.used + n.used), nextG)(Some(s)))
        }
      }
      if (n.x < maxX) {
        val rightNode = m(Point(n.x + 1, n.y))
        if (rightNode.avail >= n.used) {
          val t = s.nodes - n - rightNode
          val nextG = if (s.goalData.x == n.x && s.goalData.y == n.y) {
            Point(n.x + 1, n.y)
          } else s.goalData
          o.addOne(State(t + Node(n.name, n.x, n.y, n.size, 0) + Node(rightNode.name, n.x + 1, n.y, rightNode.size, rightNode.used + n.used), nextG)(Some(s)))
        }
      }
      if (n.y > 0) {
        val topNode = m(Point(n.x, n.y - 1))
        if (topNode.avail >= n.used) {
          val t = s.nodes - n - topNode
          val nextG = if (s.goalData.x == n.x && s.goalData.y == n.y) {
            Point(n.x, n.y - 1)
          } else s.goalData
          o.addOne(State(t + Node(n.name, n.x, n.y, n.size, 0) + Node(topNode.name, n.x, n.y - 1, topNode.size, topNode.used + n.used), nextG)(Some(s)))
        }
      }
      if (n.y < maxY) {
        val bottomNode = m(Point(n.x, n.y + 1))
        if (bottomNode.avail >= n.used) {
          val t = s.nodes - n - bottomNode
          val nextG = if (s.goalData.x == n.x && s.goalData.y == n.y) {
            Point(n.x, n.y + 1)
          } else s.goalData
          o.addOne(State(t + Node(n.name, n.x, n.y, n.size, 0) + Node(bottomNode.name, n.x, n.y + 1, bottomNode.size, bottomNode.used + n.used), nextG)(Some(s)))
        }
      }
    }

    val ns = o.result()
    println("Adding " + ns.size + " new states")
    ns
  }
}

object Day22 {
  def apply() = new Day22()

  val line: Regex = raw"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+\d+T\s+\d+%".r

  case class Node(name: String, x: Int, y: Int, size: Int, used: Int) {
    def avail: Int = size - used
  }

  case class Point(x: Int, y: Int)

  case class State(nodes: Set[Node], goalData: Point)(val parent: Option[State])

}

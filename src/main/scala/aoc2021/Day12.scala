package aoc2021

import aoc.Day

import scala.collection.mutable

class Day12 extends Day(2021, 12) {

  import Day12._

  override def part1(input: Array[String]): String = {
    val grid = Graph(mutable.Map[String, List[String]]())
    input.foreach { l =>
      val lr = l.split('-')
      grid.addEdge(lr(0), lr(1))
      grid.addEdge(lr(1), lr(0))
    }
    val paths = grid.allPaths("start", "end")
    paths.length.toString
  }

  val example1 = """start-A
                   |start-b
                   |A-c
                   |A-b
                   |b-d
                   |A-end
                   |b-end""".stripMargin.split("\n")

  val example2 = """dc-end
                   |HN-start
                   |start-kj
                   |dc-start
                   |dc-HN
                   |LN-dc
                   |HN-end
                   |kj-sa
                   |kj-HN
                   |kj-dc""".stripMargin.split("\n")

  val example3 = """fs-end
                   |he-DX
                   |fs-he
                   |start-DX
                   |pj-DX
                   |end-zg
                   |zg-sl
                   |zg-pj
                   |pj-he
                   |RW-he
                   |fs-DX
                   |pj-RW
                   |zg-RW
                   |start-pj
                   |he-WI
                   |zg-he
                   |pj-fs
                   |start-RW""".stripMargin.split("\n")

  override def part2(input: Array[String]): String = {
    val grid = Graph(mutable.Map[String, List[String]]())
    input.foreach { l =>
      val lr = l.split('-')
      grid.addEdge(lr(0), lr(1))
      grid.addEdge(lr(1), lr(0))
    }
    val paths = grid.allPaths2("start", "end")
    paths.length.toString
  }
}

object Day12 {
  def apply() = new Day12

  case class Graph(adj: mutable.Map[String, List[String]]) {
    def addEdge(s: String, e: String): Unit = {
      if (!adj.contains(s)) {
        adj += (s -> List(e))
      } else {
        adj(s) = adj(s) :+ e
      }
    }

    def allPaths(s: String, e: String): List[String] = {
      val visited = Set[String]()
      val path = Array.ofDim[String](adj.size * 2)
      innerAllPaths(s, e, visited, path, 0)
    }

    def innerAllPaths(start: String, end: String, visited: Set[String], path: Array[String], pathIndex: Int): List[String] = {
      path(pathIndex) = start
      if (start == end) {
        List(path.take(pathIndex + 1).mkString(","))
      } else {
        adj(start).flatMap { n =>
          if (n.toLowerCase != n || !visited.contains(n)) {
            val nextVisited = if (start.toLowerCase == start) {
              visited + start
            } else {
              visited
            }
            innerAllPaths(n, end, nextVisited, path, pathIndex + 1)
          } else {
            List()
          }
        }
      }
    }

    def allPaths2(s: String, e: String): List[String] = {
      val visited = Map[String, Int]("start" -> 1).withDefault(_ => 0)
      val path = Seq[String]()
      println(adj)
      innerAllPaths2(s, e, visited, true, path)
    }

    def innerAllPaths2(start: String, end: String, visited: Map[String, Int], canRevisitSmallCave: Boolean, path: Seq[String]): List[String] = {
      if (start == end) {
        List((path :+ start).toString)
      } else {
        val nextVisited: Map[String, Int] = visited + ((start, visited(start)+1))
        adj(start).flatMap { n =>
          if (
            n.toLowerCase != n || nextVisited(n) < 1 || (canRevisitSmallCave && nextVisited(n) < 2)
          ) {
            val nextCanRevisitSmallCave = canRevisitSmallCave && (n.toLowerCase != n || nextVisited(n)<1)
            innerAllPaths2(n, end, nextVisited, nextCanRevisitSmallCave, path :+ start)
          } else {
            List()
          }
        }
      }
    }
  }
}

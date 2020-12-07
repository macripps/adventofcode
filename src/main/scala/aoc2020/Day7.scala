package aoc2020

import scala.collection.mutable

case class Colour(c: String)

case class Edge(c1: Colour, c2: Colour, n: Int)

object Day7 {

  def main(): Unit = {
    val lines = readFileToIterable("aoc2020/day7.input")
    val colourGraph = lines.flatMap { line =>
      // shiny orange bags contain 3 dim cyan bags, 1 mirrored beige bag, 5 pale orange bags.
      // (colour) bags contain {# (color) bag(s),}+
      val lr = line.split(" bags contain ")
      if (lr(1).equals("no other bags.")) {
        Seq()
      } else {
        lr(1).dropRight(1).split(", ")
          .map(c => if (c.endsWith("s")) c.dropRight(5) else c.dropRight(4))
          .map(c => Edge(Colour(lr(0)), Colour(c.drop(2)), c.take(1).toInt))
      }
    }

    println("Shiny gold bags are contained within " + part1(Colour("shiny gold"), colourGraph) + " other bags")
    println("Shiny gold bags contain " + part2(Colour("shiny gold"), colourGraph) + " other bags")
  }

  def part1(search: Colour, colourGraph: Iterable[Edge]): Int = {
    val colours = mutable.Set[Colour]()
    var coloursToFind = mutable.Set(search)
    while (coloursToFind.nonEmpty) {
      val colourSearch = coloursToFind.head
      coloursToFind = coloursToFind.tail
      colourGraph.foreach { edge =>
        if (edge.c2 == colourSearch) {
          colours += edge.c1
          coloursToFind += edge.c1
        }
      }
    }
    colours.size
  }

  def part2(search: Colour, colourGraph: Iterable[Edge]): Int = {
    // doSearch returns the total number of bags, we need to cancel out the original bag by subtracting 1
    doSearch(search, colourGraph) - 1
  }

  def doSearch(search: Colour, colourGraph: Iterable[Edge]): Int = {
    val n = colourGraph.map { e =>
      if (e.c1 == search) {
        e.n * doSearch(e.c2, colourGraph)
      } else {
        0
      }
    }.sum + 1
    n
  }

}

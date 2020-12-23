package aoc2020

import aoc.Day
import aoc2020.Day23._

class Day23 extends Day(2020, 23) {
  override lazy val input = "925176834".split("")

  val cups = input.map(_.toInt).toSeq
  val minCup = cups.min

  override def part1: String = {
    var (currentNode, nodesByValue) = buildNodes(cups)
    val maxCup = 9

    (1 to 100).foreach { it =>
      val item1 = currentNode.next
      val item2 = item1.next
      val item3 = item2.next

      currentNode.next = item3.next

      var d = currentNode.value - 1
      if (d < minCup) d = maxCup
      while (item1.value == d || item2.value == d || item3.value == d) {
        d = d - 1
        if (d < minCup) d = maxCup
      }

      val dNode = nodesByValue(d)
      item3.next = dNode.next
      nodesByValue(d).next = item1

      currentNode = currentNode.next
    }

    val oneNode = nodesByValue(1)
    var nNode = oneNode.next
    var out = ""
    while (nNode != oneNode) {
      out = out + nNode.value
      nNode = nNode.next
    }
    out
  }

  override def part2: String = {
    var (currentNode, nodesByValue) = buildNodes(cups ++ Range.inclusive(10, 1_000_000))
    val maxCup = 1_000_000

    (1 to 10_000_000).foreach { it =>
      val item1 = currentNode.next
      val item2 = item1.next
      val item3 = item2.next

      currentNode.next = item3.next

      var d = currentNode.value - 1
      if (d < minCup) d = maxCup
      while (item1.value == d || item2.value == d || item3.value == d) {
        d = d - 1
        if (d < minCup) d = maxCup
      }

      val dNode = nodesByValue(d)
      item3.next = dNode.next
      nodesByValue(d).next = item1

      currentNode = currentNode.next
    }

    val node1 = nodesByValue(1)
    (node1.next.value.toLong * node1.next.next.value.toLong).toString
  }

  def buildNodes(cups: Seq[Int]): (Node, Map[Int, Node]) = {
    val cupsI = cups.reverse
    val topNode = new Node(cupsI.head)
    val nodesByValue = Map.newBuilder[Int, Node]
    nodesByValue.addOne(cupsI.head -> topNode)

    var currentNode = topNode
    cupsI.tail.foreach { n =>
      val node = new Node(n)
      node.next = currentNode
      currentNode = node
      nodesByValue.addOne(n -> node)
    }
    topNode.next = currentNode

    (currentNode, nodesByValue.result())
  }
}

object Day23 {
  def apply() = new Day23()

  class Node(val value: Int) {
    var next: Node = _

    override def toString: String = {
      "Node(" + value + ")"
    }
  }
}

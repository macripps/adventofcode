package aoc2018

import aoc.Day

class Day8 extends Day(2018, 8) {
  import Day8._

  val example = Seq("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  override def part1(input: Array[String]): String = {
    val (node, _) = readNode(input.head.split(" "), 0)
    node.sumOfMetadata.toString
  }

  def readNode(entries: Array[String], from: Int): (Node, Int) = {
    val numChildren = entries(from).toInt
    val numMetadata = entries(from + 1).toInt
    var fromN = from + 2
    val children = (1 to numChildren).map { _ =>
      val (child, f) = readNode(entries, fromN)
      fromN = f
      child
    }.toArray
    val metadata = entries.slice(fromN, fromN + numMetadata).map(_.toInt)
    (Node(children, metadata), fromN + numMetadata)
  }

  override def part2(input: Array[String]): String = {
    val (node, _) = readNode(input.head.split(" "), 0)
    node.value.toString
  }
}

object Day8 {
  def apply() = new Day8()

  case class Node(children: Array[Node], metadata: Array[Int]) {
    def sumOfMetadata: Int = metadata.sum + children.map(_.sumOfMetadata).sum

    def value: Int = {
      if (children.isEmpty) {
        metadata.sum
      } else {
        metadata.map { i =>
          if (i < children.length + 1) children(i-1).value
          else 0
        }.sum
      }
    }
  }
}

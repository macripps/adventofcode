package aoc2018

import aoc.NewDay

class Day8 extends NewDay(2018, 8) {
  import Day8._

  part(1) {
    execute { in =>
      val (node, _) = readNode(in.head.split(" "), 0)
      node.sumOfMetadata.toString
    }
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

  part(2) {
    execute { in =>
      val (node, _) = readNode(in.head.split(" "), 0)
      node.value.toString
    }
  }
}

object Day8 {
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

object Day8Main extends Day8

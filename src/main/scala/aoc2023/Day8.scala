package aoc2023

import aoc.{Maths, NewDay}

class Day8 extends NewDay(2023, 8) {

  import Day8._

  part(1) {
    test(
      """RL
        |
        |AAA = (BBB, CCC)
        |BBB = (DDD, EEE)
        |CCC = (ZZZ, GGG)
        |DDD = (DDD, DDD)
        |EEE = (EEE, EEE)
        |GGG = (GGG, GGG)
        |ZZZ = (ZZZ, ZZZ)""".stripMargin -> 2)

    test(
      """LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)""".stripMargin -> 6)

    execute { in =>
      val ig = inputGroups(in)
      val instructions = ig.head.head
      val nodes = ig.tail.head.map { line =>
        val Array(nodeName, dirs) = line.split(" = ")
        val Array(left, right) = dirs.drop(1).dropRight(1).split(", ")
        nodeName -> Node(nodeName, left, right)
      }.toMap
      moveNodeToFinish(nodes("AAA"), instructions, nodes, _.name == "ZZZ")
    }
  }

  def moveNodeToFinish(node: Node, instructions: String, nodes: Map[String, Node], end: Node => Boolean): Int = {
    var current = node
    var step = 0
    while (!end(current)) {
      val dir = instructions.charAt(Math.floorMod(step,instructions.length))
      if (dir == 'L') {
        current = nodes(current.left)
      } else if (dir == 'R') {
        current = nodes(current.right)
      }
      step = step + 1
    }
    step
  }

  part(2) {
    test(
      """LR
        |
        |11A = (11B, XXX)
        |11B = (XXX, 11Z)
        |11Z = (11B, XXX)
        |22A = (22B, XXX)
        |22B = (22C, 22C)
        |22C = (22Z, 22Z)
        |22Z = (22B, 22B)
        |XXX = (XXX, XXX)""".stripMargin -> 6)

    execute { in =>
      val ig = inputGroups(in)
      val instructions = ig.head.head
      val nodes = ig.tail.head.map { line =>
        val Array(nodeName, dirs) = line.split(" = ")
        val Array(left, right) = dirs.drop(1).dropRight(1).split(", ")
        nodeName -> Node(nodeName, left, right)
      }.toMap
      val currents = nodes.values.filter { n => n.name.endsWith("A") }.toSet
      val steps = currents.map(n => moveNodeToFinish(n, instructions, nodes, _.name.endsWith("Z")).toLong)
      Maths.lcm(steps)
    }
  }
}

object Day8 {
  case class Node(name: String, left: String, right: String)
}

object Day8Main extends Day8

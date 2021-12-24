package aoc2021

import aoc.{Day, Search}
import Day23._

import scala.collection.mutable

class Day23 extends Day(2021, 23) {
  override def part1(input: Array[String]): String = {
    val a1 = Amphipod(1, 'A', 1)
    val a2 = Amphipod(2, 'A', 1)
    val b1 = Amphipod(1, 'B', 10)
    val b2 = Amphipod(2, 'B', 10)
    val c1 = Amphipod(1, 'C', 100)
    val c2 = Amphipod(2, 'C', 100)
    val d1 = Amphipod(1, 'D', 1000)
    val d2 = Amphipod(2, 'D', 1000)

    val allAmphipods = Map(a1 -> 14, a2 -> 16, b1 -> 12, b2 -> 17, c1 -> 15, c2 -> 18, d1 -> 11, d2 -> 13)

    val state = State1(
      None,
      allAmphipods,
      Array(
        None, None, None, None, None, None, None, None, None, None, None,
        Some(d1), Some(b1),
        Some(d1), Some(a1),
        Some(c1), Some(a2),
        Some(b2), Some(c2),
      )
    )

    def goal(s: State1): Boolean = {
      s.map(0).isEmpty && s.map(1).isEmpty && s.map(2).isEmpty && s.map(3).isEmpty &&
        s.map(4).isEmpty && s.map(5).isEmpty && s.map(6).isEmpty && s.map(7).isEmpty &&
        s.map(8).isEmpty && s.map(9).isEmpty && s.map(10).isEmpty &&
        s.map(11).nonEmpty && s.map(11).get.c == 'A' &&
        s.map(12).nonEmpty && s.map(12).get.c == 'A' &&
        s.map(13).nonEmpty && s.map(13).get.c == 'B' &&
        s.map(14).nonEmpty && s.map(14).get.c == 'B' &&
        s.map(15).nonEmpty && s.map(15).get.c == 'C' &&
        s.map(16).nonEmpty && s.map(16).get.c == 'C' &&
        s.map(17).nonEmpty && s.map(17).get.c == 'D' &&
        s.map(18).nonEmpty && s.map(18).get.c == 'D'
    }

    val visited = mutable.Map[String, Int](state.flatString -> state.cost)
    var minCost = Int.MaxValue

    val q = mutable.PriorityQueue[State1](state)((s1, s2) => s2.cost.compareTo(s1.cost))
    while (q.nonEmpty) {
      val next = q.dequeue()
      if (next.cost <= minCost) {
        if (goal(next)) {
          if (next.cost < minCost) {
            minCost = next.cost
          }
        }
        val neighbours = next.nextStates
        neighbours.foreach { n =>
          if (!visited.contains(n.flatString) || visited(n.flatString) > n.cost) {
            visited(n.flatString) = n.cost
            q.addOne(n)
          }
        }
      }
    }
    minCost.toString
  }

  override def part2(input: Array[String]): String = {
    val a1 = Amphipod(1, 'A', 1)
    val a2 = Amphipod(2, 'A', 1)
    val a3 = Amphipod(3, 'A', 1)
    val a4 = Amphipod(4, 'A', 1)
    val b1 = Amphipod(1, 'B', 10)
    val b2 = Amphipod(2, 'B', 10)
    val b3 = Amphipod(3, 'B', 10)
    val b4 = Amphipod(4, 'B', 10)
    val c1 = Amphipod(1, 'C', 100)
    val c2 = Amphipod(2, 'C', 100)
    val c3 = Amphipod(3, 'C', 100)
    val c4 = Amphipod(4, 'C', 100)
    val d1 = Amphipod(1, 'D', 1000)
    val d2 = Amphipod(2, 'D', 1000)
    val d3 = Amphipod(3, 'D', 1000)
    val d4 = Amphipod(4, 'D', 1000)

    val allAmphipods = Map(
      a1 -> 14, a2 -> 21, a3 -> 24, a4 -> 26,
      b1 -> 11, b2 -> 17, b3 -> 19, b4 -> 20,
      c1 -> 15, c2 -> 16, c3 -> 22, c4 -> 25,
      d1 -> 12, d2 -> 13, d3 -> 18, d4 -> 23,
    )

    val state = State2(
      allAmphipods,
      Array(
        None, None, None, None, None, None, None, None, None, None, None,
        Some(b1), Some(d1), Some(d2), Some(a1),
        Some(c1), Some(c2), Some(b2), Some(d3),
        Some(b3), Some(b4), Some(a2), Some(c3),
        Some(d4), Some(a3), Some(c4), Some(a4),
      )
    )

    def goal(s: State2): Boolean = {
      s.map(0).isEmpty && s.map(1).isEmpty && s.map(2).isEmpty && s.map(3).isEmpty &&
        s.map(4).isEmpty && s.map(5).isEmpty && s.map(6).isEmpty && s.map(7).isEmpty &&
        s.map(8).isEmpty && s.map(9).isEmpty && s.map(10).isEmpty &&
        s.map(11).nonEmpty && s.map(11).get.c == 'A' &&
        s.map(12).nonEmpty && s.map(12).get.c == 'A' &&
        s.map(13).nonEmpty && s.map(13).get.c == 'A' &&
        s.map(14).nonEmpty && s.map(14).get.c == 'A' &&
        s.map(15).nonEmpty && s.map(15).get.c == 'B' &&
        s.map(16).nonEmpty && s.map(16).get.c == 'B' &&
        s.map(17).nonEmpty && s.map(17).get.c == 'B' &&
        s.map(18).nonEmpty && s.map(18).get.c == 'B'
    }

    print(state)

//    val visited = mutable.Map[String, Int](state.flatString -> state.cost)
    var minCost = Int.MaxValue

    val q = mutable.PriorityQueue[State2](state)((s1, s2) => s1.cost.compareTo(s2.cost))
    while (q.nonEmpty) {
      val next = q.dequeue()
      if (next.cost <= minCost) {
        println(next, next.cost, q.size)
        if (goal(next)) {
          println(next.cost)
          if (next.cost < minCost) {
            minCost = next.cost
          }
        }
        val neighbours = next.nextStates
        neighbours.foreach { n =>
          q.addOne(n)
        }
      }
    }
    minCost.toString
  }
}

object Day23 {
  def apply() = new Day23

  case class Amphipod(id: Int, c: Char, cost: Int)

  case class State1(previousMove: Option[Amphipod], locs: Map[Amphipod, Int], map: Array[Option[Amphipod]]) {
    var cost: Int = 0

    def toChar(a: Option[Amphipod]): Char = if (a.isEmpty) '.' else a.get.c

    override def toString: String = {
      "#############\n" +
        "#" + map.slice(0, 11).map(toChar).mkString + "#\n" +
        "###" + toChar(map(11)) + "#" + toChar(map(13)) + "#" + toChar(map(15)) + "#" + toChar(map(17)) + "###\n" +
        "  #" + toChar(map(12)) + "#" + toChar(map(14)) + "#" + toChar(map(16)) + "#" + toChar(map(18)) + "#\n" +
        "  #########"
    }

    def canMove(from: Int, to: Int): Boolean = {
      if (from == to) true
      else if (map(to).nonEmpty) false
      else if (to < 11 && from < to) canMove(from, to - 1)
      else if (from < 11 && to < from) canMove(from, to + 1)
      else if (to < 2) canMove(from, to + 1)
      else if (to == 2 && (from == 11 || from == 12)) canMove(from, 11)
      else if (to > 2 && to < 11 && (from == 11 || from == 12)) canMove(from, to - 1)
      else if (to < 4 && (from == 13 || from == 14 || from == 15 || from == 16 || from == 17 || from == 18)) canMove(from, to + 1)
      else if (to == 4 && (from == 13 || from == 14)) canMove(from, 13)
      else if (to > 4 && to < 11 && (from == 13 || from == 14)) canMove(from, to - 1)
      else if (to < 6 && (from == 15 || from == 16 || from == 17 || from == 18)) canMove(from, to + 1)
      else if (to == 6 && (from == 15 || from == 16)) canMove(from, 15)
      else if (to > 6 && to < 11 && (from == 15 || from == 16)) canMove(from, to - 1)
      else if (to < 8 && (from == 17 || from == 18)) canMove(from, to + 1)
      else if (to == 8 && (from == 17 || from == 18)) canMove(from, 17)
      else if (to > 8 && to < 11) canMove(from, to - 1)
      else if (to == 11) (from == 12 || (map(from).get.c == 'A' && (map(12).isEmpty || map(12).get.c == 'A') && canMove(from, 2)))
      else if (to == 12) canMove(from, 11)
      else if (to == 13) (from == 14 || (map(from).get.c == 'B' && ((map(14).isEmpty || map(14).get.c == 'B') && canMove(from, 4))))
      else if (to == 14) canMove(from, 13)
      else if (to == 15) (from == 16 || (map(from).get.c == 'C' && ((map(16).isEmpty || map(16).get.c == 'C') && canMove(from, 6))))
      else if (to == 16) canMove(from, 15)
      else if (to == 17) (from == 18) || (map(from).get.c == 'D' && ((map(18).isEmpty || map(18).get.c == 'D') && canMove(from, 8)))
      else if (to == 18) canMove(from, 17)
      else false
    }

    def nextStates: Set[State1] = {
      val movableAmphipods = locs.keys
      var ns = Set[State1]()
      movableAmphipods.foreach { pod =>
        val dests = if (locs(pod) < 11) {
          if (pod.c == 'A') Seq(11, 12)
          else if (pod.c == 'B') Seq(13, 14)
          else if (pod.c == 'C') Seq(15, 16)
          else Seq(17, 18)
        } else if (pod.c == 'A' && locs(pod) == 12) Seq()
        else if (pod.c == 'A' && locs(pod) == 11 && map(12).nonEmpty && map(12).get.c == 'A') Seq()
        else if (pod.c == 'B' && locs(pod) == 14) Seq()
        else if (pod.c == 'B' && locs(pod) == 13 && map(14).nonEmpty && map(14).get.c == 'B') Seq()
        else if (pod.c == 'C' && locs(pod) == 16) Seq()
        else if (pod.c == 'C' && locs(pod) == 15 && map(16).nonEmpty && map(16).get.c == 'C') Seq()
        else if (pod.c == 'D' && locs(pod) == 18) Seq()
        else if (pod.c == 'D' && locs(pod) == 17 && map(18).nonEmpty && map(18).get.c == 'D') Seq()
        else (0 to 18).filter(i => i != 2 && i != 4 && i != 6 && i != 8 && i != locs(pod) && map(i).isEmpty)
        dests.foreach { newPos =>
          val oldPos = locs(pod)
          if (canMove(oldPos, newPos)) {
            val nextMap = Array.from(map)
            nextMap(locs(pod)) = None
            nextMap(newPos) = Some(pod)
            val nextLocations = locs + (pod -> newPos)
            val nextState = State1(Some(pod), nextLocations, nextMap)
            nextState.cost = cost + distance(oldPos, newPos) * pod.cost
            ns = ns + nextState
          }
        }
      }
      ns
    }

    def flatString: String = map.map(toChar).mkString

    def distance(a: Int, b: Int): Int = {
      if (a == b) 0
      else if (a < 11 && b < 11) math.abs(a - b)
      else (a, b) match {
        case (11, 12) | (12, 11) => 1
        case (13, 14) | (14, 13) => 1
        case (15, 16) | (16, 15) => 1
        case (17, 18) | (18, 17) => 1
        case (11, _) => 1 + distance(2, b)
        case (13, _) => 1 + distance(4, b)
        case (15, _) => 1 + distance(6, b)
        case (17, _) => 1 + distance(8, b)
        case (12, _) => 1 + distance(11, b)
        case (14, _) => 1 + distance(13, b)
        case (16, _) => 1 + distance(15, b)
        case (18, _) => 1 + distance(17, b)
        case (_, 11) => 1 + distance(a, 2)
        case (_, 13) => 1 + distance(a, 4)
        case (_, 15) => 1 + distance(a, 6)
        case (_, 17) => 1 + distance(a, 8)
        case (_, 12) => 1 + distance(a, 11)
        case (_, 14) => 1 + distance(a, 13)
        case (_, 16) => 1 + distance(a, 15)
        case (_, 18) => 1 + distance(a, 17)
      }
    }
  }


  case class State2(locs: Map[Amphipod, Int], map: Array[Option[Amphipod]]) {
    var cost: Int = 0

    def toChar(a: Option[Amphipod]): Char = if (a.isEmpty) '.' else a.get.c

    override def toString: String = {
      "#############\n" +
        "#" + map.slice(0, 11).map(toChar).mkString + "#\n" +
        "###" + toChar(map(11)) + "#" + toChar(map(15)) + "#" + toChar(map(19)) + "#" + toChar(map(23)) + "###\n" +
        "  #" + toChar(map(12)) + "#" + toChar(map(16)) + "#" + toChar(map(20)) + "#" + toChar(map(24)) + "#\n" +
        "  #" + toChar(map(13)) + "#" + toChar(map(17)) + "#" + toChar(map(21)) + "#" + toChar(map(25)) + "#\n" +
        "  #" + toChar(map(14)) + "#" + toChar(map(18)) + "#" + toChar(map(22)) + "#" + toChar(map(26)) + "#\n" +
        "  #########"
    }

    def canMove(from: Int, to: Int): Boolean = {
      if (from == to) true
      else if (map(to).nonEmpty) false
      else if (to < 11 && from < to) canMove(from, to - 1)
      else if (from < 11 && to < from) canMove(from, to + 1)
      else if (to < 2) canMove(from, to + 1)
      else if (to == 2 && (from > 10 && from < 15)) canMove(from, 11)
      else if (to > 2 && to < 11 && from > 10 && from < 15) canMove(from, to - 1)
      else if (to < 4 && from > 14) canMove(from, to + 1)
      else if (to == 4 && from > 14 && from < 19) canMove(from, 15)
      else if (to > 4 && to < 11 && from > 14 && from < 19) canMove(from, to - 1)
      else if (to < 6 && from > 18) canMove(from, to + 1)
      else if (to == 6 && from > 18 && from < 23) canMove(from, 19)
      else if (to > 6 && to < 11 && from > 18 && from < 23) canMove(from, to - 1)
      else if (to < 8 && from > 22) canMove(from, to + 1)
      else if (to == 8 && from > 22) canMove(from, 23)
      else if (to > 8 && to < 11) canMove(from, to - 1)
      else if (to == 11) (from == 12 || (map(from).get.c == 'A' && (map(12).isEmpty || map(12).get.c == 'A') && (map(13).isEmpty || map(13).get.c == 'A') && (map(14).isEmpty || map(14).get.c == 'A') && canMove(from, 2)))
      else if (to == 12) (from == 13 || (map(from).get.c == 'A' && (map(13).isEmpty || map(13).get.c == 'A') && (map(14).isEmpty || map(14).get.c == 'A') && canMove(from, 11)))
      else if (to == 13) (from == 14 || (map(from).get.c == 'A' && (map(14).isEmpty || map(14).get.c == 'A') && canMove(from, 12)))
      else if (to == 14) canMove(from, 13)
      else if (to == 15) (from == 16 || (map(from).get.c == 'B' && ((map(14).isEmpty || map(14).get.c == 'B') && canMove(from, 4))))
      else if (to == 16) (from == 17 || (map(from).get.c == 'B' && ((map(14).isEmpty || map(14).get.c == 'B') && canMove(from, 15))))
      else if (to == 17) (from == 18 || (map(from).get.c == 'B' && ((map(14).isEmpty || map(14).get.c == 'B') && canMove(from, 16))))
      else if (to == 18) canMove(from, 17)
      else if (to == 19) (from == 20 || (map(from).get.c == 'C' && ((map(16).isEmpty || map(16).get.c == 'C') && canMove(from, 6))))
      else if (to == 20) (from == 21 || (map(from).get.c == 'C' && ((map(16).isEmpty || map(16).get.c == 'C') && canMove(from, 19))))
      else if (to == 21) (from == 22 || (map(from).get.c == 'C' && ((map(16).isEmpty || map(16).get.c == 'C') && canMove(from, 20))))
      else if (to == 22) canMove(from, 21)
      else if (to == 23) (from == 24) || (map(from).get.c == 'D' && ((map(18).isEmpty || map(18).get.c == 'D') && canMove(from, 8)))
      else if (to == 24) (from == 25) || (map(from).get.c == 'D' && ((map(18).isEmpty || map(18).get.c == 'D') && canMove(from, 23)))
      else if (to == 25) (from == 26) || (map(from).get.c == 'D' && ((map(18).isEmpty || map(18).get.c == 'D') && canMove(from, 24)))
      else if (to == 26) canMove(from, 25)
      else false
    }

    def nextStates: Set[State2] = {
      val movableAmphipods = locs.keys
      var ns = Set[State2]()
      movableAmphipods.foreach { pod =>
        val dests = if (locs(pod) < 11) {
          if (pod.c == 'A') Seq(11, 12, 13, 14)
          else if (pod.c == 'B') Seq(15, 16, 17, 18)
          else if (pod.c == 'C') Seq(19, 20, 21, 22)
          else Seq(23, 24, 25, 26)
        } else if (pod.c == 'A' && locs(pod) == 14) Seq()
        else if (pod.c == 'A' && locs(pod) == 11 && map(12).nonEmpty && map(12).get.c == 'A' && map(13).nonEmpty && map(13).get.c == 'A' && map(14).nonEmpty && map(14).get.c == 'A') Seq()
        else if (pod.c == 'A' && locs(pod) == 12 && map(13).nonEmpty && map(13).get.c == 'A' && map(14).nonEmpty && map(14).get.c == 'A') Seq()
        else if (pod.c == 'A' && locs(pod) == 13 && map(14).nonEmpty && map(14).get.c == 'A') Seq()
        else if (pod.c == 'B' && locs(pod) == 18) Seq()
        else if (pod.c == 'B' && locs(pod) == 15 && map(16).nonEmpty && map(16).get.c == 'B' && map(17).nonEmpty && map(17).get.c == 'B' && map(18).nonEmpty && map(18).get.c == 'B') Seq()
        else if (pod.c == 'B' && locs(pod) == 16 && map(17).nonEmpty && map(17).get.c == 'B' && map(18).nonEmpty && map(18).get.c == 'B') Seq()
        else if (pod.c == 'B' && locs(pod) == 17 && map(18).nonEmpty && map(18).get.c == 'B') Seq()
        else if (pod.c == 'C' && locs(pod) == 22) Seq()
        else if (pod.c == 'C' && locs(pod) == 19 && map(20).nonEmpty && map(20).get.c == 'C' && map(21).nonEmpty && map(21).get.c == 'C' && map(22).nonEmpty && map(22).get.c == 'C') Seq()
        else if (pod.c == 'C' && locs(pod) == 20 && map(21).nonEmpty && map(21).get.c == 'C' && map(22).nonEmpty && map(22).get.c == 'C') Seq()
        else if (pod.c == 'C' && locs(pod) == 21 && map(22).nonEmpty && map(22).get.c == 'C') Seq()
        else if (pod.c == 'D' && locs(pod) == 26) Seq()
        else if (pod.c == 'D' && locs(pod) == 23 && map(24).nonEmpty && map(24).get.c == 'D' && map(25).nonEmpty && map(25).get.c == 'D' && map(26).nonEmpty && map(26).get.c == 'D') Seq()
        else if (pod.c == 'D' && locs(pod) == 24 && map(25).nonEmpty && map(25).get.c == 'D' && map(26).nonEmpty && map(26).get.c == 'D') Seq()
        else if (pod.c == 'D' && locs(pod) == 25 && map(26).nonEmpty && map(26).get.c == 'D') Seq()
        else (0 to 26).filter(i => i != 2 && i != 4 && i != 6 && i != 8 && i != locs(pod) && map(i).isEmpty)
        dests.foreach { newPos =>
          val oldPos = locs(pod)
          if (canMove(oldPos, newPos)) {
            val nextMap = Array.from(map)
            nextMap(locs(pod)) = None
            nextMap(newPos) = Some(pod)
            val nextLocations = locs + (pod -> newPos)
            val nextState = State2(nextLocations, nextMap)
            nextState.cost = cost + distance(oldPos, newPos) * pod.cost
            ns = ns + nextState
          }
        }
      }
      ns
    }

    def flatString: String = map.map(toChar).mkString

    def distance(a: Int, b: Int): Int = {
      if (a == b) 0
      else if (a < 11 && b < 11) math.abs(a - b)
      else (a, b) match {
        case (11, 12) | (12, 11) => 1
        case (12, 13) | (13, 12) => 1
        case (13, 14) | (14, 13) => 1
        case (11, 13) | (13, 11) => 2
        case (12, 14) | (14, 12) => 2
        case (11, 14) | (14, 11) => 3
        case (15, 16) | (16, 15) => 1
        case (16, 17) | (17, 16) => 1
        case (17, 18) | (18, 17) => 1
        case (15, 17) | (17, 15) => 2
        case (16, 18) | (18, 16) => 2
        case (15, 18) | (18, 15) => 3
        case (19, 20) | (20, 19) => 1
        case (20, 21) | (21, 20) => 1
        case (21, 22) | (22, 21) => 1
        case (19, 21) | (21, 19) => 2
        case (20, 22) | (22, 20) => 2
        case (19, 22) | (22, 19) => 3
        case (23, 24) | (24, 23) => 1
        case (24, 25) | (25, 24) => 1
        case (25, 26) | (26, 25) => 1
        case (23, 25) | (25, 23) => 2
        case (24, 26) | (26, 24) => 2
        case (23, 26) | (26, 23) => 3
        case (11, _) => 1 + distance(2, b)
        case (12, _) => 1 + distance(11, b)
        case (13, _) => 1 + distance(12, b)
        case (14, _) => 1 + distance(13, b)
        case (15, _) => 1 + distance(4, b)
        case (16, _) => 1 + distance(15, b)
        case (17, _) => 1 + distance(16, b)
        case (18, _) => 1 + distance(17, b)
        case (19, _) => 1 + distance(6, b)
        case (20, _) => 1 + distance(19, b)
        case (21, _) => 1 + distance(20, b)
        case (22, _) => 1 + distance(21, b)
        case (23, _) => 1 + distance(8, b)
        case (24, _) => 1 + distance(23, b)
        case (25, _) => 1 + distance(24, b)
        case (26, _) => 1 + distance(25, b)
        case (_, 11) => 1 + distance(a, 2)
        case (_, 12) => 1 + distance(a, 11)
        case (_, 13) => 1 + distance(a, 12)
        case (_, 14) => 1 + distance(a, 13)
        case (_, 15) => 1 + distance(a, 4)
        case (_, 16) => 1 + distance(a, 15)
        case (_, 17) => 1 + distance(a, 16)
        case (_, 18) => 1 + distance(a, 17)
        case (_, 19) => 1 + distance(a, 6)
        case (_, 20) => 1 + distance(a, 19)
        case (_, 21) => 1 + distance(a, 20)
        case (_, 22) => 1 + distance(a, 21)
        case (_, 23) => 1 + distance(a, 8)
        case (_, 24) => 1 + distance(a, 23)
        case (_, 25) => 1 + distance(a, 24)
        case (_, 26) => 1 + distance(a, 25)
      }
    }
  }
}

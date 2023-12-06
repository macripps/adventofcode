package aoc2022

import scala.collection.mutable
import Day16._

import scala.util.control.Breaks.{break, breakable}

class Day16 extends aoc.Day(2022, 16) {

  val test =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
      |Valve HH has flow rate=22; tunnel leads to valve GG
      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
      |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

  withPart1Test(test, 1651)

  override def part1(input: Array[String]): Any = {
    val valves = mutable.Map[String, Valve]()
    input.foreach {
      case line(id: String, rate: String, neighbours: String) =>
        val valve = new Valve(id, rate.toInt, neighbours.split(", ").toSet)
        valves(id) = valve
    }
    val start = "AA"
    val open = Set[String]()
    search(valves.toMap, start, Set(), open, 30, 0)
  }

  var bestSoFar = Int.MinValue

  private[this] def search(valves: Map[String, Valve], location: String, visited: Set[String], open: Set[String], timeRemaining: Int, currentFlow: Int): Int = {
    if (timeRemaining == 0) {
      if (currentFlow > bestSoFar) {
        bestSoFar = currentFlow
      }
      currentFlow
    } else if (!valves.keySet.diff(open).exists(id => valves(id).rate > 0)) {
      // No more to open
      val result = currentFlow + (open.map { o => valves(o).rate * timeRemaining }.sum)
      if (result > bestSoFar) {
        bestSoFar = result
      }
      result
    } else {
      val theoreticalMax = currentFlow + open.map(o =>
        valves(o).rate * timeRemaining
      ).sum + (valves(location).rate * timeRemaining) + (valves.keySet.diff(open) - location).map { couldBeOpened =>
        valves(couldBeOpened).rate * (timeRemaining - 1)
      }.sum
      if (theoreticalMax < bestSoFar) {
        0
      } else {
        val openScore = if (valves(location).rate > 0 && !open.contains(location)) {
          // Spend one move opening valve in current location
          search(
            valves, location, Set(), open + location, timeRemaining - 1, currentFlow + open.map(id => valves(id).rate).sum)
        } else {
          Int.MinValue
        }
        // Move somewhere else
        val valve = valves(location)
        math.max(
          openScore,
          valve.neighbours.diff(visited).toSeq.sortBy(id => -valves(id).rate).map { neighbour =>
            search(valves, neighbour, visited + location, open, timeRemaining - 1, currentFlow + open.map(id => valves(id).rate).sum)
          }.maxOption.getOrElse(Int.MinValue)
        )
      }
    }
  }

  withPart2Test(test, 1707)

  override def part2(input: Array[String]): Any = {
    val valves = mutable.Map[String, Valve]()
    input.foreach {
      case line(id: String, rate: String, neighbours: String) =>
        val valve = new Valve(id, rate.toInt, neighbours.split(", ").toSet)
        valves(id) = valve
    }
    doubleSearch(valves.toMap)
  }


  case class State(time: Int, locationA: String, locationB: String, currentFlow: Int, opened: Set[String])

  private[this] def doubleSearch(valves: Map[String, Valve]): Int = {
    val maxFlow = valves.values.map(_.rate).sum
    val start = "AA"
    val states = mutable.ArrayDeque[State](State(1, start, start, 0, Set()))
    val seen = mutable.Map[(Int, String, String), Int]().withDefaultValue(Int.MinValue)
    var doubleBestSoFar = Int.MinValue
    while (states.nonEmpty) {
      breakable {
        val current = states.removeHead()
        val time = current.time
        val locationA = current.locationA
        val locationB = current.locationB
        val score = current.currentFlow
        val opened = current.opened
        val currentFlow = opened.map(id => valves(id).rate).sum

        if (seen((time, locationA, locationB)) >= score) {
          break()
        }
        seen((time, locationA, locationB)) = score

        if (time == 26) {
          if (score > doubleBestSoFar) {
            doubleBestSoFar = score
            println("New best: " + doubleBestSoFar)
          }
          break()
        }

        if (currentFlow == maxFlow) {
          // No more to open
          val result = score + (currentFlow * (26 - time))
          states.append(State(26, locationA, locationB, result, opened))
        } else {
          // A can move or open
          // B can move or open
          val aCanOpen = valves(locationA).rate > 0 && !opened.contains(locationA)
          val bCanOpen = valves(locationB).rate > 0 && !opened.contains(locationB)
          val aMoves = valves(locationA).neighbours
          val bMoves = valves(locationB).neighbours
          val nextFlow = score + opened.map(id => valves(id).rate).sum
          if (aCanOpen && bCanOpen && locationA != locationB) {
            states.append(State(time + 1, locationA, locationB, nextFlow + valves(locationA).rate + valves(locationB).rate, opened + locationA + locationB))
          } else if (aCanOpen) {
            bMoves.toSeq.foreach { neighbourB =>
              states.append(State(time + 1, locationA, neighbourB, nextFlow + valves(locationA).rate, opened + locationA))
            }
          } else if (bCanOpen) {
            aMoves.toSeq.foreach { neighbourA =>
              states.append(State(time + 1, neighbourA, locationB, nextFlow + valves(locationB).rate, opened + locationB))
            }
          }
          // Just moves
          aMoves.toSeq.foreach { neighbourA =>
            bMoves.toSeq.foreach { neighbourB =>
              states.append(State(time + 1, neighbourA, neighbourB, nextFlow, opened))
            }
          }
        }
      }
    }
    doubleBestSoFar
  }
}

object Day16 {
  def apply() = new Day16

  val line = raw"Valve (.+?) has flow rate=(\d+); tunnels? leads? to valves? (.+)".r
  //             Valve AA    has flow rate=0    ; tunnels lead to valves    DD, II, BB
}

class Valve(name: String, val rate: Int, val neighbours: Set[String]) {
}

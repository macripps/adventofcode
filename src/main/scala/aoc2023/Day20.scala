package aoc2023

import aoc.{Maths, NewDay}

import scala.collection.mutable

class Day20 extends NewDay(2023, 20) {

  part(1) {
    test {
      """broadcaster -> a, b, c
        |%a -> b
        |%b -> c
        |%c -> inv
        |&inv -> a""".stripMargin -> 32000000L
    }
    test {
      """broadcaster -> a
        |output -> none
        |%a -> inv, con
        |&inv -> b
        |%b -> con
        |&con -> output""".stripMargin -> 11687500L
    }
    execute { input =>
      var modules = input.map {
        Module.from
      }.map { m => m.name -> m }.toMap

      modules = modules + ("rx" -> Output("rx", List()))

      modules.values.foreach { m =>
        m.destinations.foreach { d =>
          modules(d) match {
            case conjunction: Conjunction =>
              conjunction.inputs(m.name) = Low
            case _ =>
          }
        }
      }

      var lowCount = 0L
      var highCount = 0L
      var push = 1
      while (push <= 1000) {
        val pulses = mutable.Queue[(String, String, Pulse)]()
        pulses.enqueue(("button", "broadcaster", Low))
        while (pulses.nonEmpty) {
          val pulse = pulses.dequeue()
          if (pulse._3 == High) {
            highCount = highCount + 1
          } else {
            lowCount = lowCount + 1
          }
          modules(pulse._2) match {
            case b: Broadcast =>
              b.destinations.foreach { d =>
                pulses.enqueue((b.name, d, pulse._3))
              }
            case f: FlipFlop =>
              if (pulse._3 == High) {
                // Ignore
              } else {
                val pulseToSend = if (f.onOff) Low else High
                f.onOff = !f.onOff
                f.destinations.foreach { d =>
                  pulses.enqueue((f.name, d, pulseToSend))
                }
              }
            case c: Conjunction =>
              c.inputs(pulse._1) = pulse._3
              val allHigh = c.inputs.values.forall(_ == High)
              c.destinations.foreach { d =>
                if (allHigh) {
                  pulses.enqueue((c.name, d, Low))
                } else {
                  pulses.enqueue((c.name, d, High))
                }
              }
            case o: Output =>
          }
        }
        val backToStart = modules.values.forall {
          case b: Broadcast => true
          case o: Output => true
          case c: Conjunction =>
            c.inputs.values.forall(_ == Low)
          case f: FlipFlop => f.onOff == false
        }
        if (backToStart) {
          val factor = (1000 / push)
          val skip = factor * push
          highCount = highCount * (factor)
          lowCount = lowCount * (factor)
          push = push + skip
        }
        push = push + 1
      }
      highCount * lowCount
    }
  }

  part(2) {
    execute { input =>
      var modules = input.map {
        Module.from
      }.map { m => m.name -> m }.toMap

      val l = Listener("rx", List())

      modules = modules + ("rx" -> l)

      modules.values.foreach { m =>
        m.destinations.foreach { d =>
          modules(d) match {
            case conjunction: Conjunction =>
              conjunction.inputs(m.name) = Low
            case _ =>
          }
        }
      }

      var phCycle = 0L
      var vnCycle = 0L
      var ktCycle = 0L
      var hnCycle = 0L

      var push = 0L
      while (phCycle == 0 || vnCycle == 0 || ktCycle == 0 || hnCycle == 0) {
        val pulses = mutable.Queue[(String, String, Pulse)]()
        pulses.enqueue(("button", "broadcaster", Low))
        push = push + 1L
        while (pulses.nonEmpty) {
          val pulse = pulses.dequeue()
          if (pulse._3 == Low && (pulse._2 == "ph" || pulse._2 == "vn" || pulse._2 == "kt" || pulse._2 == "hn")) {
            if (pulse._2 == "ph" && phCycle == 0) {
              phCycle = push
            } else if (pulse._2 == "vn" && vnCycle == 0) {
              vnCycle = push
            } else if (pulse._2 == "kt" && ktCycle == 0) {
              ktCycle = push
            } else if (pulse._2 == "hn" && hnCycle == 0) {
              hnCycle = push
            }
          }
          modules(pulse._2) match {
            case b: Broadcast =>
              b.destinations.foreach { d =>
                pulses.enqueue((b.name, d, pulse._3))
              }
            case f: FlipFlop =>
              if (pulse._3 == High) {
                // Ignore
              } else {
                val pulseToSend = if (f.onOff) Low else High
                f.onOff = !f.onOff
                f.destinations.foreach { d =>
                  pulses.enqueue((f.name, d, pulseToSend))
                }
              }
            case c: Conjunction =>
              c.inputs(pulse._1) = pulse._3
              val allHigh = c.inputs.values.forall(_ == High)
              c.destinations.foreach { d =>
                if (allHigh) {
                  pulses.enqueue((c.name, d, Low))
                } else {
                  pulses.enqueue((c.name, d, High))
                }
              }
            case _: Output =>
            case l: Listener =>
              l.seenLowInput |= pulse._3 == Low
          }
        }
      }
      Maths.lcm(List(phCycle, vnCycle, ktCycle, hnCycle))
    }
  }

}

sealed trait Module {
  val name: String
  val destinations: List[String]
}

object Module {
  def from(s: String): Module = {
    val Array(name, dests) = s.split(" -> ")
    val destsList = dests.split(", ").toList
    if (name == "broadcaster") {
      Broadcast(name, destsList)
    } else if (name == "output") {
      Output(name, List())
    } else if (name.charAt(0) == '%') {
      FlipFlop(name.drop(1), destsList)
    } else if (name.charAt(0) == '&') {
      Conjunction(name.drop(1), destsList)
    } else {
      throw new IllegalArgumentException("Unable to build Module from " + s)
    }
  }
}

case class Broadcast(override val name: String, override val destinations: List[String]) extends Module

case class FlipFlop(override val name: String, override val destinations: List[String]) extends Module {
  // false = off
  var onOff: Boolean = false
}

case class Conjunction(override val name: String, override val destinations: List[String]) extends Module {
  val inputs: mutable.Map[String, Pulse] = mutable.Map[String, Pulse]()
}

case class Output(override val name: String, override val destinations: List[String]) extends Module

case class Listener(override val name: String, override val destinations: List[String]) extends Module {
  var seenLowInput = false
}

sealed trait Pulse

case object Low extends Pulse

case object High extends Pulse

object Day20Main extends Day20

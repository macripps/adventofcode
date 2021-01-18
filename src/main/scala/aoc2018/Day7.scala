package aoc2018

import aoc.Day

import scala.collection.mutable

class Day7 extends Day(2018, 7) {

  import Day7._

  val example =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".stripMargin.split("\n")

  override def part1: String = {
    var ordering = input.map {
      case line(before: String, after: String) => (before, after)
    }.groupMapReduce(kv => kv._1)(kv => Set(kv._2))(_ ++ _)
    var out = ""
    val steps = mutable.Set[String]()
    steps.addAll(('A' to 'Z').map(_.toString))
    while (steps.nonEmpty) {
      val availSteps = steps.diff(ordering.values.flatten.toSet)
      val minStep = availSteps.min
      steps -= minStep
      out = out + minStep
      ordering = ordering - minStep
    }
    out
  }

  override def part2: String = {
    val maxWorkers = 5
    var ordering = input.map {
      case line(before: String, after: String) => (before, after)
    }.groupMapReduce(kv => kv._1)(kv => Set(kv._2))(_ ++ _)
    val workers = Array.fill[Option[(String, Int, Int)]](maxWorkers)(None)
    var out = ""
    val steps = mutable.Set[String]()
    steps.addAll(('A' to 'Z').map(_.toString))
    var t = 0
    if (debug) {
      println("Second Worker 1 Worker 2 Worker 3 Worker 4 Worker 5 Done")
    }
    while (t == 0 || steps.nonEmpty || workers.exists(_.isDefined)) {
      workers.zipWithIndex.foreach { k =>
        val i = k._2
        if (k._1.isDefined && k._1.get._3 == t) {
          out = out + k._1.get._1
          steps -= k._1.get._1
          ordering = ordering - k._1.get._1
          workers(i) = None
        }
      }
      val availSteps = steps.diff(ordering.values.flatten.toSet).diff(workers.map(_.map(_._1).getOrElse("")).toSet)
      while (availSteps.nonEmpty && workers.exists(_.isEmpty)) {
        val minStep = availSteps.min
        (0 until maxWorkers).find(workers(_).isEmpty).foreach { i =>
          workers(i) = Some(minStep, t, t + 60 + minStep.charAt(0) + 1 - 'A')
          availSteps -= minStep
        }
      }
      if (debug) {
        println(("% 4d      %s        %s        %s        %s        %s       %s").format(t,
          workers(0).map(_._1).getOrElse("."),
          workers(1).map(_._1).getOrElse("."),
          workers(2).map(_._1).getOrElse("."),
          workers(3).map(_._1).getOrElse("."),
          workers(4).map(_._1).getOrElse("."),
          out
        ))
      }
      t = t + 1
    }
    (t - 1).toString
  }
}

object Day7 {
  def apply() = new Day7()

  val line = raw"Step (\w) must be finished before step (\w) can begin.".r
}

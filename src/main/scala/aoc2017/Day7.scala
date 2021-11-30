package aoc2017

import aoc.Day

import scala.collection.mutable
import scala.util.matching.Regex

class Day7 extends Day(2017, 7) {
  import Day7._

  def programs(input: Array[String]): Array[Program] = input map {
    case top(name: String, weight: String) =>
      Program(name, weight.toInt, List())
    case line(name: String, weight: String, right: String, last: String, x: String) =>
      Program(name, weight.toInt, right.split(", ").toList)
  }

  override def part1(input: Array[String]): String = {
    val ps = programs(input)
    ps.find(p => !ps.exists(_.holding.contains(p.name))).map(_.name).get
  }

  override def part2(input: Array[String]): String = {
    val programs = input map {
      case top(name: String, weight: String) =>
        Program(name, weight.toInt, List())
      case line(name: String, weight: String, right: String, last: String, x: String) =>
        Program(name, weight.toInt, right.split(", ").toList)
    }
    val pMap = programs.map { p => p.name -> p }.toMap

    def recWeight(p: Program): Int = {
      p.weight + p.holding.map(p => recWeight(pMap(p))).sum
    }
    val root = programs.find(p => !programs.exists(_.holding.contains(p.name))).get

    val openSet = mutable.Queue((0, root))
    val nodePath = mutable.Buffer[Program]()
    while (openSet.nonEmpty) {
      val n = openSet.dequeue()
      nodePath.addOne(n._2)
      val depth = n._1
      val node = n._2
      val heaviest = node.holding.maxBy { p =>
        recWeight(pMap(p))
      }
      if (!node.holding.forall{n => recWeight(pMap(n)) == recWeight(pMap(heaviest))}) {
        openSet.addOne(depth+1, pMap(heaviest))
      }
    }
    val tooHeavy = nodePath.last
    val lightest = nodePath.dropRight(1).last.holding.minBy { p => recWeight(pMap(p))}
    (tooHeavy.weight + recWeight(pMap(lightest)) - recWeight(tooHeavy)).toString
  }
}

object Day7 {
  def apply() = new Day7()

  val top: Regex = raw"(\w+) \((\d+)\)".r
  val line: Regex = raw"(\w+) \((\d+)\) -> ((\w+, )*(\w+))".r
}

case class Program(name: String, weight: Int, holding: List[String])

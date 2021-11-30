package aoc2015

import aoc.Day
import Day24._

import scala.collection.mutable

class Day24 extends Day(2015, 24) {
//  override lazy val input =
//    """1
//      |2
//      |3
//      |4
//      |5
//      |7
//      |8
//      |9
//      |10
//      |11
//      |""".stripMargin.split("\n")

  override def part1(input: Array[String]): String = {
    val weights = input.map(_.toInt).toSeq
    val totalWeight = weights.sum
    val options = findThreeOptionsThatSumTo(weights, totalWeight/3)
    options.product.toString
  }

  override def part2(input: Array[String]): String = {
    val weights = input.map(_.toInt).toSeq
    val totalWeight = weights.sum
    val options = findThreeOptionsThatSumTo(weights, totalWeight/4)
    options.product.toString
  }
}

object Day24 {
  def apply() = new Day24()

  def findThreeOptionsThatSumTo(input: Seq[Int], target: Int): Seq[Long] = {
    println("Each compartment must weigh " + target)

    val options = mutable.Buffer[Long]()
    subsetSum(input, target, Seq(), options)
    options.toSeq
  }

  def subsetSum(numbers: Seq[Int], target: Int, current: Seq[Long], shortest: mutable.Buffer[Long]): Unit = {
    val s = current.sum
    if (s == target) {
      if (shortest.isEmpty || current.size < shortest.size || (current.size == shortest.size && current.product < shortest.product)) {
        shortest.clear()
        shortest.addAll(current)
      }
    } else if (s < target && (shortest.isEmpty || (shortest.nonEmpty && current.size < shortest.size))) {
      numbers.indices.foreach { n =>
        val remaining = numbers.drop(n + 1)
        subsetSum(remaining, target, current :+ numbers(n), shortest)
      }
    }
  }
}

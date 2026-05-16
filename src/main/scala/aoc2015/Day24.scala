package aoc2015

import aoc.NewDay
import Day24._

import scala.collection.mutable

class Day24 extends NewDay(2015, 24) {
  part(1) {
    execute { in =>
      val weights = in.map(_.toInt).toSeq
      val totalWeight = weights.sum
      val options = findThreeOptionsThatSumTo(weights, totalWeight/3)
      options.product.toString
    }
  }

  part(2) {
    execute { in =>
      val weights = in.map(_.toInt).toSeq
      val totalWeight = weights.sum
      val options = findThreeOptionsThatSumTo(weights, totalWeight/4)
      options.product.toString
    }
  }
}

object Day24 {
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

object Day24Main extends Day24

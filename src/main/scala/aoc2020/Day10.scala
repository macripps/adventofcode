package aoc2020

import aoc.Day

import scala.collection.mutable

class Day10 extends Day(2020, 10) {
  val adapters = input.map(_.toInt).sorted

  override def part1: String = {
    var current = 0
    var oneJDifferences = 0
    var threeJDifferences = 1
    adapters.foreach { a =>
      if (a - current == 1) {
        oneJDifferences = oneJDifferences + 1
      } else if (a - current == 3) {
        threeJDifferences = threeJDifferences + 1
      }
      current = a
    }
    "Product is " + (oneJDifferences * threeJDifferences)
  }

  override def part2: String = {
    val permutations = Day10.part2(0 +: adapters :+ (adapters.max + 3))
    "Permutations: " + permutations
  }
}

object Day10 {
  def apply() = new Day10()

  def part2(adapters: Array[Int]): Long = {
    cache.clear()
    iPart2(adapters, 0)
  }

  private[this] val cache = mutable.Map[Int, Long]()

  def iPart2(adapters: Array[Int], start: Int): Long = {
    if (cache.contains(start)) {
      cache(start)
    } else {
      if (start == adapters.length - 1) {
        1
      } else {
        var perms = 0L
        (start + 1 until adapters.length).foreach { i =>
          if (adapters(i) - adapters(start) <= 3) {
            // This adapter is optional
            perms = perms + iPart2(adapters, i)
          }
        }
        cache(start) = perms
        perms
      }
    }
  }
}

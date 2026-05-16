package aoc2020

import aoc.NewDay

import scala.collection.mutable

class Day10 extends NewDay(2020, 10) {
  def adapters(input: Array[String]): Array[Int] = input.map(_.toInt).sorted

  part(1) {
    execute { in =>
      var current = 0
      var oneJDifferences = 0
      var threeJDifferences = 1
      adapters(in).foreach { a =>
        if (a - current == 1) {
          oneJDifferences = oneJDifferences + 1
        } else if (a - current == 3) {
          threeJDifferences = threeJDifferences + 1
        }
        current = a
      }
      "Product is " + (oneJDifferences * threeJDifferences)
    }
  }

  part(2) {
    execute { in =>
      val adpts = adapters(in)
      val permutations = Day10.part2(0 +: adpts :+ (adpts.max + 3))
      "Permutations: " + permutations
    }
  }
}

object Day10Main extends Day10

object Day10 {
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

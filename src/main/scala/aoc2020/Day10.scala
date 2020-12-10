package aoc2020

import scala.collection.mutable

object Day10 {

  def main(): Unit = {
    val adapters = readFileToIterable("aoc2020/day10.input").toArray.map(_.toInt).sorted
    part1(adapters)
    val permutations = part2(0 +: adapters :+ (adapters.max + 3))
    println("Permutations: " + permutations)
  }

  def part1(adapters: Array[Int]) = {
    var current = 0
    var oneJDifferences = 0
    var threeJDifferences = 1
    adapters.foreach { a =>
      println(a)
      if (a - current == 1) {
        oneJDifferences = oneJDifferences + 1
      } else if (a - current == 3) {
        threeJDifferences = threeJDifferences + 1
      }
      current = a
    }
    println("OneJDifferences: " + oneJDifferences)
    println("ThreeJDifferences: " + threeJDifferences)
    println("Product is " + (oneJDifferences * threeJDifferences))
  }

  def part2(adapters: Array[Int]): Long = {
    cache.clear()
    iPart2(adapters, 0)
  }

  val cache = mutable.Map[Int, Long]()

  def iPart2(adapters: Array[Int], start: Int): Long = {
    if (cache.contains(start)) {
      println("From cache: " + start + ": " + cache(start))
      cache(start)
    } else {
    if (start == adapters.length - 1) {
      1
    } else {
      var perms = 0L
      (start + 1 until adapters.length).foreach { i =>
        if (adapters(i) - adapters(start) <= 3) {
          // This adapter is optional
          println("Swizzling " + adapters(i))
          perms = perms + iPart2(adapters, i)
        }
      }
      println("Perms:" + perms)
      cache(start) = perms
      perms
    }
      //    var i = start
      //    var perms: Long = 1
      //    while (i < adapters.length - 1) {
      //      var j = i + 1
      //      var maxRemoved = i + 1
      //      while (j < adapters.length - 1) {
      //        if (adapters(j + 1) - adapters(i) <= 3) {
      //          println("Dropping " + j)
      //          perms = perms + iPart2(adapters.take(j) ++ adapters.drop(j+1), j - 1)
      //          maxRemoved = j + 1
      //        }
      //        j = j + 1
      //      }
      //      i = maxRemoved
      //    }
      //    println("Permutation found: " + adapters.mkString("Array(", ",", ")"))
    }
  }

}

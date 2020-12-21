package aoc2015

import aoc.Day
import Day15._

class Day15 extends Day(2015, 15) {
  override def part1: String = {
    val cookies = input.map {
      case recipe(name, capacity, durability, flavor, texture, calories) => (name, capacity.toLong, durability.toLong, flavor.toLong, texture.toLong, calories.toLong)
    }

    var proportions = Array.ofDim[Int](cookies.length)
    proportions(cookies.length - 1) = 100
    var results = 0L
    while (proportions(0) < 100) {
      val product: Long = score(proportions, cookies)
      if (product > results) {
        results = product
      }
      proportions = nextDown(proportions)
    }
    results.toString
  }

  override def part2: String = {
    val cookies = input.map {
      case recipe(name, capacity, durability, flavor, texture, calories) => (name, capacity.toLong, durability.toLong, flavor.toLong, texture.toLong, calories.toLong)
    }

    var proportions = Array.ofDim[Int](cookies.length)
    proportions(cookies.length - 1) = 100
    var results = 0L
    while (proportions(0) < 100) {
      if (calories(proportions, cookies) == 500) {
        val product: Long = score(proportions, cookies)
        if (product > results) {
          results = product
        }
      }
      proportions = nextDown(proportions)
    }
    results.toString
  }
}

object Day15 {
  def apply() = new Day15()

  val recipe = raw"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)".r

  def nextDown(props: Array[Int]): Array[Int] = {
    val rest = nextUp(props.dropRight(1))
    rest :+ (100 - rest.sum)
  }

  def nextUp(props: Array[Int]): Array[Int] = {
    if (props.sum != 100) {
      props.dropRight(1) :+ (props(props.length - 1) + 1)
    } else {
      val rest = nextUp(props.dropRight(1))
      rest :+ 0
    }
  }

  def score(proportions: Array[Int], cookies: Array[(String, Long, Long, Long, Long, Long)]): Long = {
    val scores = proportions.indices.map { i =>
      (
        proportions(i) * cookies(i)._2,
        proportions(i) * cookies(i)._3,
        proportions(i) * cookies(i)._4,
        proportions(i) * cookies(i)._5,
      )
    }
    val capacity = scores.map(_._1).sum.max(0L)
    val durability = scores.map(_._2).sum.max(0L)
    val flavor = scores.map(_._3).sum.max(0L)
    val texture = scores.map(_._4).sum.max(0L)
    capacity * durability * flavor * texture
  }

  def calories(proportions: Array[Int], cookies: Array[(String, Long, Long, Long, Long, Long)]): Long = {
    proportions.indices.map { i => proportions(i) * cookies(i)._6 }.sum
  }
}

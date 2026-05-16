package aoc2015

import aoc.NewDay
import Day15._

import scala.util.matching.Regex

class Day15 extends NewDay(2015, 15) {
  part(1) {
    execute { in =>
      val cookies = in.map {
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
  }

  part(2) {
    execute { in =>
      val cookies = in.map {
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
}

object Day15 {
  val recipe: Regex = raw"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)".r

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

object Day15Main extends Day15

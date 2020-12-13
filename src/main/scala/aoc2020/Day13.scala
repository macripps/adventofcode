package aoc2020

import aoc.Day

class Day13 extends Day {
  override def year: Int = 2020
  override def day: Int = 13

  override def part1(input: Array[String]): String = {
    val timestamp = input(0).toInt
    val busIds = input(1).split(",").filterNot(_ == "x").map(_.toInt)
    val firstBus = busIds.map(id => (id, id - timestamp % id)).minBy(f => f._2)
    "First bus with id " + firstBus._1 + " after waiting " + firstBus._2 + " minutes (product = " + (firstBus._1 * firstBus._2) + ")"
  }

  override def part2(input: Array[String]): String = {
    val busIds = input(1).split(",").zipWithIndex.filterNot(_._1 == "x").map(p => (((p._1.toInt - (p._2 % p._1.toInt)) % p._1.toInt), p._1.toInt))
    val result = Day13.chineseRemainerTheorem(busIds)
    "x = " + result._1 + " (mod " + result._2 + ")"
  }
}

object Day13 {
  def apply() = new Day13

  def chineseRemainerTheorem(remainderModuliPairs: Array[(Int, Int)]): (Long, Long) = {
    var remainder: BigInt = remainderModuliPairs(0)._1
    var modulus: Long = remainderModuliPairs(0)._2
    remainderModuliPairs.drop(1).foreach { e =>
      val bz = bezoutIdentity(modulus, e._2)
      remainder = (remainder * e._2 * bz._2) + (modulus * e._1 * bz._1)
      modulus = modulus * e._2 / bz._3
      remainder = remainder % modulus
    }
    if (remainder < 0) {
      remainder = modulus + remainder
    }
    (remainder.longValue, modulus)
  }

  /**
   * Returns three numbers such that a * _1 + b * _2 = _3 (gcd(a,b))
   */
  def bezoutIdentity(a: Long, b: Long): (Long, Long, Long) = {
    var s0 = 1: Long
    var s1 = 0: Long
    var t0 = 0: Long
    var t1 = 1: Long
    var r0 = a
    var r1 = b
    while (r1 != 0) {
      val q = r0 / r1
      val r = r0 % r1
      val s = s0 - q * s1
      val t = t0 - q * t1
      s0 = s1
      s1 = s
      t0 = t1
      t1 = t
      r0 = r1
      r1 = r
    }
    (s0, t0, r0)
  }
}

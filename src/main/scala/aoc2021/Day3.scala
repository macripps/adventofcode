package aoc2021

import aoc.Day

class Day3 extends Day(2021, 3) {
  override def part1(input: Array[String]): String = {
    val z = input.map(_.toCharArray).transpose
    val y = z.map(new String(_))
    var episilon = ""
    var gamma = ""
    y.foreach { x =>
      val zs = x.count(_ == '0')
      val ones = x.count(_ == '1')
      if (zs > ones) {
        gamma += "0"
        episilon += "1"
      } else {
        gamma += "1"
        episilon += "0"
      }
    }
    val g = Integer.parseInt(gamma, 2)
    val e = Integer.parseInt(episilon, 2)
    println(gamma)
    println(episilon)
    (g * e).toString
  }

  val example =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin.split("\n")

  override def part2(input: Array[String]): String = {
    var zs = input
    var x: Int = 0
    while (zs.length > 1) {
      val o = zs.map(_.toCharArray).transpose.map(new String(_))
      val os = o(x).count(x => x == '0')
      val is = o(x).count(x => x == '1')
      if (os > is) {
        zs = zs.filter(_.charAt(x) == '0')
      } else {
        zs = zs.filter(_.charAt(x) == '1')
      }
      x = x + 1
    }
    val ox = zs.head

    zs = input
    x = 0
    while (zs.length > 1) {
      val o = zs.map(_.toCharArray).transpose.map(new String(_))
      val os = o(x).count(x => x == '0')
      val is = o(x).count(x => x == '1')
      if (is < os) {
        zs = zs.filter(_.charAt(x) == '1')
      } else {
        zs = zs.filter(_.charAt(x) == '0')
      }
      x = x + 1
    }
    val co = zs.head
    println(ox)
    println(co)

    val n1 = Integer.parseInt(ox, 2)
    val n2 = Integer.parseInt(co, 2)
    (n1 * n2).toString
  }
}

object Day3 {
  def apply() = new Day3()
}

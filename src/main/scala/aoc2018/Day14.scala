package aoc2018

import aoc.Day

import scala.collection.mutable

class Day14 extends Day(2018, 14) {
  override lazy val input: Array[String] = Array("327901")

  override def part1: String = {
    val i = input.head.toInt
    val array = mutable.Buffer[Byte](3, 7)
    var i0 = 0
    var i1 = 1
    while (array.length < i + 10) {
      val n = array(i0) + array(i1)
      if (n >= 10) {
        array.append((n / 10).toByte)
      }
      array.append((n % 10).toByte)
      i0 = (i0 + 1 + array(i0)) % array.length
      i1 = (i1 + 1 + array(i1)) % array.length
    }
    array.slice(i, i + 10).mkString("")
  }

  override def part2: String = {
    val i = input.head.split("").map(_.toInt)
    val array = mutable.Buffer[Byte](3, 7)
    var i0 = 0
    var i1 = 1
    val window = mutable.ArrayDeque[Byte](3, 7)
    while (!window.containsSlice(i)) {
      val n = array(i0) + array(i1)
      if (n >= 10) {
        if (window.size == i.length) {
          window.removeHead()
        }
        array.append((n / 10).toByte)
        window.append((n / 10).toByte)
      }
      if (!window.containsSlice(i)) {
        if (window.size == i.length) {
          window.removeHead()
        }
        array.append((n % 10).toByte)
        window.append((n % 10).toByte)
      }
      i0 = (i0 + 1 + array(i0)) % array.length
      i1 = (i1 + 1 + array(i1)) % array.length
    }
    (array.length - i.length).toString
  }
}

object Day14 {
  def apply() = new Day14()
}

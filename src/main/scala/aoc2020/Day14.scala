package aoc2020

import aoc.Day

import scala.collection.mutable

class Day14 extends Day {
  override def year: Int = 2020

  override def day: Int = 14

  override def part1(input: Array[String]): String = {
    val memory = mutable.Map[Long, Long]()
    var mask = ""
    input.foreach { line =>
      if (line.startsWith("mask = ")) {
        mask = line.drop(7)
      } else if (line.startsWith("mem[")) {
        val end = line.indexOf(']')
        val loc = line.substring(4, end).toLong
        val value = line.drop(end + 4).toLong

        memory(loc) = Day14.applyValueMask(mask, value)
      }
    }
    "The sum of memory is " + (memory.values.sum)
  }

  override def part2(input: Array[String]): String = {
    val memory = mutable.Map[Long, Long]()
    var mask = ""
    input.foreach { line =>
      if (line.startsWith("mask = ")) {
        mask = line.drop(7)
      } else if (line.startsWith("mem[")) {
        val end = line.indexOf(']')
        val loc = line.substring(4, end).toLong
        val value = line.drop(end + 4).toLong

        Day14.applyMemoryMask(mask, loc).foreach { loc =>
          memory(loc) = value
        }
      }
    }
    "The sum of memory is " + (memory.values.sum)
  }
}

object Day14 {
  def apply() = new Day14()

  def applyValueMask(mask: String, value: Long): Long = {
    val out = toBinaryStringOfLength(value, 36)
    val res = mask.zipWithIndex.map {
      case ('0', i) => '0'
      case ('1', i) => '1'
      case ('X', i) => out.charAt(i)
    }.mkString
    java.lang.Long.parseUnsignedLong(res, 2)
  }

  def applyMemoryMask(mask: String, location: Long): Iterable[Long] = {
    val ln = toBinaryStringOfLength(location, 36)
    val res = mask.zipWithIndex.map {
      case ('0', i) => ln.charAt(i)
      case ('1', i) => '1'
      case ('X', i) => 'X'
    }.mkString
    val xs = mask.count(_ == 'X')
    val lns = (0 until 1 << xs).map { i =>
      val opts = toBinaryStringOfLength(i, xs)

      var o = res
      opts.foreach { c =>
        o = o.replaceFirst("X", "" + c)
      }
      java.lang.Long.parseUnsignedLong(o, 2)
    }

    lns
  }

  def toBinaryStringOfLength(i: Long, length: Int): String = {
    var out = i.toBinaryString
    if (out.length < length) {
      out = ("0" * length) + out
    }
    out.takeRight(length)
  }
}

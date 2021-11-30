package aoc2015

import aoc.Day
import Day7._

import scala.collection.mutable

class Day7 extends Day(2015, 7) {
  override def part1(input: Array[String]): String = {
    cache.clear()
    val wires = input.map(l => {
      val io = l.split(" -> ")
      io(1) -> io(0)
    }).toMap
    calc(wires, "a").toString
  }

  override def part2(input: Array[String]): String = {
    val bVal = part1(input)
    cache.clear()
    val wires = input.map(l => {
      val io = l.split(" -> ")
      io(1) -> io(0)
    }).toMap
    calc(wires + ("b" -> bVal), "a").toString
  }
}

object Day7 {
  def apply() = new Day7()

  val cache: mutable.Map[String, Short] = mutable.Map[String, Short]()

  def calc(wires: Map[String, String], desired: String): Short = {
    if (cache.contains(desired)) {
      cache(desired)
    } else {
      desired.toShortOption match {
        case Some(v) =>
          cache(desired) = v
          v
        case None =>
          val rhs = wires(desired)
          val o = rhs match {
            case Not(v) => ~calc(wires, v)
            case And(l, r) => calc(wires, l) & calc(wires, r)
            case Or(l, r) => calc(wires, l) | calc(wires, r)
            case LShift(l, n) => calc(wires, l) << n
            case RShift(l, n) => calc(wires, l) >> n
            case n => calc(wires, n)
          }
          cache(desired) = o.toShort
          o.toShort
      }
    }
  }


  object Not {
    def unapply(v: String): Option[String] = {
      if (v.startsWith("NOT ")) {
        Some(v.substring(4))
      } else None
    }
  }

  object And {
    def unapply(v: String): Option[(String, String)] = {
      if (v.contains(" AND ")) {
        val strings = v.split(" AND ")
        Some(strings(0), strings(1))
      } else None
    }
  }

  object Or {
    def unapply(v: String): Option[(String, String)] = {
      if (v.contains(" OR ")) {
        val strings = v.split(" OR ")
        Some(strings(0), strings(1))
      } else None
    }
  }

  object LShift {
    def unapply(v: String): Option[(String, Int)] = {
      if (v.contains(" LSHIFT ")) {
        val strings = v.split(" LSHIFT ")
        Some(strings(0), strings(1).toInt)
      } else None
    }
  }

  object RShift {
    def unapply(v: String): Option[(String, Int)] = {
      if (v.contains(" RSHIFT ")) {
        val strings = v.split(" RSHIFT ")
        Some(strings(0), strings(1).toInt)
      } else None
    }
  }
}

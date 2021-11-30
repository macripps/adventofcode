package aoc2016

import aoc.Day

class Day19 extends Day(2016, 19) {
  import Day19._
  override def part1(input: Array[String]): String = {
    val elves = input(0).toInt
    val l = elves - Integer.highestOneBit(elves)
    val safe = 2 * l + 1
    safe.toString
  }

  override def part2(input: Array[String]): String = {
    val elves = input(0).toInt
    var e = new Elf(1)
    val first = e
    var half: Option[Elf] = None
    (2 to elves).foreach { i =>
      val e1 = new Elf(i)
      if (i == (elves+1)/2) {
        half = Some(e1)
      }
      e1.prev = e
      e.next = e1
      e = e1
    }
    first.prev = e
    e.next = first

    var h = half.get

    var zap = elves
    var elf = first
    while (elf.next != elf) {
      h.prev.next = h.next
      h.next.prev = h.prev
      h = h.next
      if (zap % 2 == 1) {
        h = h.next
      }
      elf = elf.next
      zap = zap - 1
    }
    elf.i.toString
  }
}

object Day19 {
  def apply() = new Day19()

  class Elf(val i: Int) {
    var prev: Elf = _
    var next: Elf = _
  }
}

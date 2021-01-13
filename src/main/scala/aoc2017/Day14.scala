package aoc2017

import aoc.{Day, Point}

import scala.collection.mutable

class Day14 extends Day(2017, 14) {
  override def part1: String = {
    knotHash(input.head).map { x => x.map(Integer.bitCount).sum }.sum.toString
  }

  override def part2: String = {
    val result = knotHash(input.head)
    val grid = Array.ofDim[Boolean](128, 128)
    var y = 0
    result.foreach { rows =>
      var x = 0
      rows.foreach { i =>
        if ((i & 0x80) == 0x80) {
          grid(y)(x) = true
        }
        if ((i & 0x40) == 0x40) {
          grid(y)(x + 1) = true
        }
        if ((i & 0x20) == 0x20) {
          grid(y)(x + 2) = true
        }
        if ((i & 0x10) == 0x10) {
          grid(y)(x + 3) = true
        }
        if ((i & 0x8) == 0x8) {
          grid(y)(x + 4) = true
        }
        if ((i & 0x4) == 0x4) {
          grid(y)(x + 5) = true
        }
        if ((i & 0x2) == 0x2) {
          grid(y)(x + 6) = true
        }
        if ((i & 0x1) == 1) {
          grid(y)(x + 7) = true
        }
        x = x + 8
      }
      y = y + 1
    }
    var groups = 0
    (0 until 128).foreach { y =>
      (0 until 128).foreach { x =>
        if (grid(y)(x)) {
          groups = groups + 1
          val q = mutable.Queue[Point](Point(x, y))
          while (q.nonEmpty) {
            val p = q.dequeue()
            grid(p.y)(p.x) = false
            q.addAll(p.neighbours.filter{p2 => p2.x>=0 && p2.x<128 && p2.y>=0 && p2.y<128 && grid(p2.y)(p2.x)})
          }
        }
      }
    }
    groups.toString
  }

  def knotHash(input: String) = {
    (0 to 127).map { i =>
      Day10.knotHash(input + "-" + i)
    }
  }
}

object Day14 {
  def apply() = new Day14()
}

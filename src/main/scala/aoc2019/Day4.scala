package aoc2019

import aoc.Day

class Day4 extends Day(2019, 4) {
  override def part1(input: Array[String]): String = {
    (152085 to 670283).count { i =>
      val n = i.toString
      if (n.charAt(0) == n.charAt(1) ||
        n.charAt(1) == n.charAt(2) ||
        n.charAt(2) == n.charAt(3) ||
        n.charAt(3) == n.charAt(4) ||
        n.charAt(4) == n.charAt(5)) {
        if (n.charAt(0) <= n.charAt(1) &&
          n.charAt(1) <= n.charAt(2) &&
          n.charAt(2) <= n.charAt(3) &&
          n.charAt(3) <= n.charAt(4) &&
          n.charAt(4) <= n.charAt(5)) {
          true
        } else false
      } else false
    }.toString
  }

  override def part2(input: Array[String]): String = {
    (152085 to 670283).count(p => matches2(p.toString)).toString
  }

  def matches2(n: String): Boolean = {
    if (n.charAt(0) > n.charAt(1) ||
      n.charAt(1) > n.charAt(2) ||
      n.charAt(2) > n.charAt(3) ||
      n.charAt(3) > n.charAt(4) ||
      n.charAt(4) > n.charAt(5)) {
      false
    } else if ((n.charAt(0) != n.charAt(1)) &&
      (n.charAt(1) != n.charAt(2)) &&
      (n.charAt(2) != n.charAt(3)) &&
      (n.charAt(3) != n.charAt(4)) &&
      n.charAt(4) != n.charAt(5)) {
      false
    } else {
      if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) false
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) false
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) == n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) false
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) == n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) false
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) == n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) == n.charAt(5)) false
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) == n.charAt(4) && n.charAt(4) != n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) == n.charAt(5)) true
      else if (n.charAt(0) != n.charAt(1) && n.charAt(1) != n.charAt(2) && n.charAt(2) != n.charAt(3) && n.charAt(3) != n.charAt(4) && n.charAt(4) != n.charAt(5)) false
      else false
    }
  }
}

object Day4 {
  def apply() = new Day4
}

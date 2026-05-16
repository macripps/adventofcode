package aoc2019

import aoc.NewDay

class Day4 extends NewDay(2019, 4) {
  part(1) {
    execute { _ =>
      (152085 to 670283).count { i =>
        val n = i.toString
        n.charAt(0) <= n.charAt(1) &&
          n.charAt(1) <= n.charAt(2) &&
          n.charAt(2) <= n.charAt(3) &&
          n.charAt(3) <= n.charAt(4) &&
          n.charAt(4) <= n.charAt(5) &&
          (n.charAt(0) == n.charAt(1) ||
            n.charAt(1) == n.charAt(2) ||
            n.charAt(2) == n.charAt(3) ||
            n.charAt(3) == n.charAt(4) ||
            n.charAt(4) == n.charAt(5))
      }
    }
  }

  part(2) {
    execute { _ =>
      (152085 to 670283).count(p => matches2(p.toString))
    }
  }

  def matches2(n: String): Boolean = {
    if (n.charAt(0) > n.charAt(1) ||
      n.charAt(1) > n.charAt(2) ||
      n.charAt(2) > n.charAt(3) ||
      n.charAt(3) > n.charAt(4) ||
      n.charAt(4) > n.charAt(5)) {
      false
    } else {
      // Must have at least one run of exactly 2
      val runs = n.toList.foldRight(List.empty[(Char, Int)]) {
        case (c, (hc, hn) :: tail) if c == hc => (hc, hn + 1) :: tail
        case (c, acc) => (c, 1) :: acc
      }
      runs.exists(_._2 == 2)
    }
  }
}

object Day4Main extends Day4

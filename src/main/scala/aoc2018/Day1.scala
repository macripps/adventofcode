package aoc2018

import aoc.NewDay

import scala.collection.mutable

class Day1 extends NewDay(2018, 1) {
  part(1) {
    execute { in =>
      in.map(_.toInt).sum.toString
    }
  }

  part(2) {
    execute { in =>
      val i = in.map(_.toInt)
      val seen = mutable.Set[Int]()
      var x = 0
      var p = 0
      while (!seen.contains(x)) {
        seen.addOne(x)
        val n = i(p % i.length)
        x = x + n
        p = p + 1
      }
      x.toString
    }
  }
}

object Day1Main extends Day1

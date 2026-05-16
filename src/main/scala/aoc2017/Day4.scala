package aoc2017

import aoc.NewDay

class Day4 extends NewDay(2017, 4) {
  part(1) {
    execute { in =>
      in.map { l =>
        val x = l.split(" ")
        x.toSet.size == x.length
      }.count(x => x).toString
    }
  }

  part(2) {
    execute { in =>
      in.map { l =>
        val x = l.split(" ").map(_.toCharArray.sorted.mkString)
        x.toSet.size == x.length
      }.count(x => x).toString
    }
  }

}

object Day4Main extends Day4

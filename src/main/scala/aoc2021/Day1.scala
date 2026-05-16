package aoc2021

import aoc.NewDay

class Day1 extends NewDay(2021, 1) {
  part(1) {
    execute { in =>
      in.zip(in.tail).count(increasing).toString
    }
  }

  part(2) {
    execute { in =>
      in.zip(in.drop(3)).count(increasing).toString
    }
  }

  private[this] def increasing(e: (String, String)): Boolean = {
    e._2.toInt > e._1.toInt
  }
}

object Day1Main extends Day1

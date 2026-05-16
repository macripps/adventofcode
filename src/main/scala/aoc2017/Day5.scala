package aoc2017

import aoc.NewDay

class Day5 extends NewDay(2017, 5) {
  part(1) {
    execute { in =>
      val prog = in.map(_.toInt)
      var steps = 0
      var ep = 0
      while (ep >= 0 && ep < prog.length) {
        val offset = prog(ep)
        prog(ep) = offset + 1
        ep = ep + offset
        steps = steps + 1
      }
      steps.toString
    }
  }

  part(2) {
    execute { in =>
      val prog = in.map(_.toInt)
      var steps = 0
      var ep = 0
      while (ep >= 0 && ep < prog.length) {
        val offset = prog(ep)
        prog(ep) = if (offset >= 3) offset - 1 else offset + 1
        ep = ep + offset
        steps = steps + 1
      }
      steps.toString
    }
  }
}

object Day5Main extends Day5

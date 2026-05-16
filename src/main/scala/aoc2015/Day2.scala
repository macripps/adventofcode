package aoc2015

import aoc.NewDay

class Day2 extends NewDay(2015, 2) {
  part(1) {
    execute { in =>
      in.map { line =>
        line.split("x").map(_.toInt).sorted match {
          case Array(l, w, h) => (2*l*w) + (2*w*h) + (2*h*l) + (l*w)
        }
      }.sum.toString
    }
  }

  part(2) {
    execute { in =>
      in.map { line =>
        line.split("x").map(_.toInt).sorted match {
          case Array(l, w, h) => (2*l) + (2*w) + (w*h*l)
        }
      }.sum.toString
    }
  }
}

object Day2Main extends Day2

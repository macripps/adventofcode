package aoc2019

import aoc.NewDay

class Day8 extends NewDay(2019, 8) {
  part(1) {
    execute { in =>
      val minLayer = in.head.grouped(25 * 6).minBy(_.count(_ == '0'))
      minLayer.count(_ == '1') * minLayer.count(_ == '2')
    }
  }

  part(2) {
    execute { in =>
      val layers = in.head.grouped(25 * 6).toSeq
      "\n" + (0 to 5).map { row =>
        (0 to 24).map { col =>
          val idx = row * 25 + col
          val pixel = layers.dropWhile(_.charAt(idx) == '2').head.charAt(idx)
          if (pixel == '1') '#' else ' '
        }.mkString
      }.mkString("\n")
    }
  }
}

object Day8Main extends Day8

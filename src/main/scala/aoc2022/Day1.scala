package aoc2022

import aoc.NewDay

class Day1 extends NewDay(2022, 1) {
  part(1) {
    execute { in =>
      aoc.asGroupsSeparatedByBlankLines(in).map(x => x.map(_.toInt).sum).max.toString
    }
  }

  part(2) {
    execute { in =>
      aoc.asGroupsSeparatedByBlankLines(in).map(x => x.map(_.toInt).sum).toSeq.sorted.takeRight(3).sum.toString
    }
  }
}

object Day1Main extends Day1

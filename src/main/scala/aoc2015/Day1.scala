package aoc2015

import aoc.NewDay

class Day1 extends NewDay(2015, 1) {
  part(1) {
    execute { in =>
      "The final floor is " + in(0).map[Int](c => if (c == '(') 1 else -1).sum
    }
  }

  part(2) {
    execute { in =>
      val s = in(0).map[Int](c => if (c == '(') 1 else -1)
      val pos = s.indices.find(i => s.take(i).sum < 0).get
      "" + pos
    }
  }
}

object Day1Main extends Day1

package aoc2016

import aoc.NewDay

class Day3 extends NewDay(2016, 3) {
  part(1) {
    execute { in =>
      in.count { l =>
        val a = Seq(l.substring(2, 5).replace(" ", ""), l.substring(7, 10).replace(" ", ""), l.substring(12, 15).replace(" ", "")).map(_.toInt).sorted
        a.head + a(1) > a(2)
      }.toString
    }
  }

  part(2) {
    execute { in =>
      in.grouped(3).flatMap { l =>
        Seq(
          Seq(l(0).substring(2, 5).replace(" ", ""), l(1).substring(2, 5).replace(" ", ""), l(2).substring(2, 5).replace(" ", "")).map(_.toInt).sorted,
          Seq(l(0).substring(7, 10).replace(" ", ""), l(1).substring(7, 10).replace(" ", ""), l(2).substring(7, 10).replace(" ", "")).map(_.toInt).sorted,
          Seq(l(0).substring(12, 15).replace(" ", ""), l(1).substring(12, 15).replace(" ", ""), l(2).substring(12, 15).replace(" ", "")).map(_.toInt).sorted
        )
      }.count { a => a.head + a(1) > a(2) }.toString
    }
  }
}

object Day3Main extends Day3

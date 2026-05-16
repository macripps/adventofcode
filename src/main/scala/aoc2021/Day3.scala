package aoc2021

import aoc.NewDay

class Day3 extends NewDay(2021, 3) {
  part(1) {
    execute { in =>
      val (gamma, epsilon) = in.map(_.toCharArray)
        .transpose
        .map { x =>
        if (x.count(_ == '0') > x.count(_ == '1')) {
          (0, 1)
        } else {
          (1, 0)
        }
      }.reduceLeft[(Int,Int)]((x,y) => (x._1 + x._1 + y._1, x._2 + x._2 + y._2))
      (gamma * epsilon).toString
    }
  }

  part(2) {
    execute { in =>
      var zs = in
      var x: Int = 0
      while (zs.length > 1) {
        val (os, is) = zs.partition(z => z.charAt(x) == '0')
        if (os.length > is.length) {
          zs = os
        } else {
          zs = is
        }
        x = x + 1
      }
      val ox = zs.head

      zs = in
      x = 0
      while (zs.length > 1) {
        val (os, is) = zs.partition(z => z.charAt(x) == '0')
        if (is.length < os.length) {
          zs = is
        } else {
          zs = os
        }
        x = x + 1
      }
      val co = zs.head

      val n1 = Integer.parseInt(ox, 2)
      val n2 = Integer.parseInt(co, 2)
      (n1 * n2).toString
    }
  }
}

object Day3Main extends Day3

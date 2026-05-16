package aoc2018

import aoc.{NewDay, Strings}

class Day2 extends NewDay(2018, 2) {
  part(1) {
    execute { in =>
      val wcs = in.map { w =>
        w.groupMapReduce(identity)(_ => 1)(_ + _)
      }
      (wcs.count(_.exists(x => x._2 == 2)) * wcs.count(_.exists(x => x._2 == 3))).toString
    }
  }

  part(2) {
    execute { in =>
      in.combinations(2).filter(xs => Strings.levenshteinDistance(xs(0), xs(1)) == 1).map { xs =>
        xs(0).zipWithIndex.filter(p => xs(1).charAt(p._2) == p._1).map(_._1).mkString
      }.mkString(",")
    }
  }
}

object Day2Main extends Day2

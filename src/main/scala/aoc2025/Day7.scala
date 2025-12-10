package aoc2025

import aoc.NewDay
import io.opentelemetry.api.GlobalOpenTelemetry

import scala.collection.mutable

class Day7 extends NewDay(2025, 7) {
  part(1) {
    test(""".......S.......
           |...............
           |.......^.......
           |...............
           |......^.^......
           |...............
           |.....^.^.^.....
           |...............
           |....^.^...^....
           |...............
           |...^.^...^.^...
           |...............
           |..^...^.....^..
           |...............
           |.^.^.^.^.^...^.
           |...............""".stripMargin -> 21)
    execute { ls =>
      val start = ls.head.indexOf('S')
      val beams = mutable.HashSet[Int](start)
      var splits = 0
      ls.foreach { l =>
        l.zipWithIndex.filter(i => i._1 == '^' && beams.contains(i._2)).foreach { i =>
          beams.remove(i._2)
          beams.add(i._2 - 1)
          beams.add(i._2 + 1)
          splits = splits + 1
        }
      }
      splits
    }
  }

  part(2) {
    test(""".......S.......
           |...............
           |.......^.......
           |...............
           |......^.^......
           |...............
           |.....^.^.^.....
           |...............
           |....^.^...^....
           |...............
           |...^.^...^.^...
           |...............
           |..^...^.....^..
           |...............
           |.^.^.^.^.^...^.
           |...............""".stripMargin -> 40)
    execute { ls =>
      val start = ls.head.indexOf('S')

      val cache = mutable.HashMap[(Int, Int), Long]()

      def depthFirst(pos: Int, idx: Int, rows: Array[String]): Long = {
        if (cache.contains((pos, idx))) {
          cache((pos, idx))
        } else {
          if (idx == rows.length) {
            cache((pos, idx)) = 1L
            1L
          } else if (rows(idx)(pos) == '^') {
            val routes = depthFirst(pos-1, idx, rows) + depthFirst(pos+1, idx,rows)
            cache((pos, idx)) = routes
            routes
          } else {
            val routes = depthFirst(pos, idx+2, rows)
            cache((pos, idx)) = routes
            routes
          }
        }
      }

      depthFirst(start, 0, ls)
    }
  }
}

object Day7Main extends Day7
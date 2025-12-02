package aoc2025

import aoc.NewDay

class Day2 extends NewDay(2025, 2) {
  part(1) {
    test("""11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124""".stripMargin -> 1227775554L)
    execute { in =>
      in.head.split(',').map { s =>
        val Array(l, h) = s.split('-').map(_.toLong)
        println(l,h,h-l)
      }
    }
  }
}

object Day2Main extends Day2

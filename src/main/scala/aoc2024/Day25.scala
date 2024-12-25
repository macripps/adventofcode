package aoc2024

import aoc.NewDay

import scala.collection.mutable

class Day25 extends NewDay(2024, 25) {
  part(1) {
    test("""#####
           |.####
           |.####
           |.####
           |.#.#.
           |.#...
           |.....
           |
           |#####
           |##.##
           |.#.##
           |...##
           |...#.
           |...#.
           |.....
           |
           |.....
           |#....
           |#....
           |#...#
           |#.#.#
           |#.###
           |#####
           |
           |.....
           |.....
           |#.#..
           |###..
           |###.#
           |###.#
           |#####
           |
           |.....
           |.....
           |.....
           |#....
           |#.#..
           |#.#.#
           |#####""".stripMargin -> 3)
    execute { ls =>
      val schematics = aoc.asGroupsSeparatedByBlankLines(ls)
      val keys = mutable.ListBuffer[Array[Int]]()
      val locks = mutable.ListBuffer[Array[Int]]()
      var size = 0
      schematics.foreach { s =>
        size = s.size - 2
        if (s.head.forall(c => c == '#')) {
          keys += s.transpose.map { r => r.count(_ == '#') - 1}.toArray
        } else {
          locks += s.transpose.map { r => r.count(_ == '#') - 1}.toArray
        }
      }
      keys.map { k =>
        locks.count { l =>
          k.zip(l).forall { kl => kl._1 + kl._2 <= size}
        }
      }.sum
    }
  }
}

object Day25Main extends Day25

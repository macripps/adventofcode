package aoc2025

import aoc.NewDay

import scala.collection.mutable

class Day10 extends NewDay(2025, 10) {

  part(1) {
    test("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" -> 2)
    test("[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" -> 3)
    test("[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" -> 2)
    execute { ls =>
      ls.map { l =>
        val split = l.split(' ')
        val desiredLights = split.head.drop(1).dropRight(1).map(c => c == '#')
        val wiring = split.drop(1).dropRight(1).map { w =>
          w.drop(1).dropRight(1).split(',').map(_.toInt).toSet
        }.toSet
        wiring.subsets().toSet.filter { ws =>
          val lighting = Array.ofDim[Boolean](desiredLights.length)
          ws.foreach { buttons =>
            buttons.foreach { idx =>
              lighting(idx) = !lighting(idx)
            }
          }
          lighting.sameElements(desiredLights)
        }.minBy(_.size).size
      }.sum
    }
  }

  part(2) {
    test("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" -> 10)
    test("[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" -> 12)
    test("[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" -> 11)
    execute { ls =>
      ls.map { l =>
        val split = l.split(' ')
        val wiring = split.drop(1).dropRight(1).map { w =>
          w.drop(1).dropRight(1).split(',').map(_.toInt).toSet
        }.sortBy(- _.size)
        val desiredJoltage = split.last.drop(1).dropRight(1).split(',').map(_.toInt)

        val search = mutable.Queue[List[Set[Int]]](List())
        def joltageOf(buttons: List[Set[Int]]): Array[Int] = {
          val joltage = Array.ofDim[Int](desiredJoltage.length)
          buttons.foreach { bs =>
            bs.foreach { b =>
              joltage(b) = joltage(b) + 1
            }
          }
          joltage
        }
        while (!desiredJoltage.sameElements(joltageOf(search.front))) {
          val current = search.dequeue()
          val dj = joltageOf(current)
          println(search.size, current, dj.mkString("(", ",", ")"), desiredJoltage.mkString("(", ",", ")"))
          if (dj.zipWithIndex.forall {case (j, i) => j <= desiredJoltage(i)}) {
            wiring.foreach { b =>
              search.enqueue(current :+ b)
            }
          }
        }
        search.front.size
      }.sum
    }
  }
}

object Day10Main extends Day10

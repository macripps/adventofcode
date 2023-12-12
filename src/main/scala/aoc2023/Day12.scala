package aoc2023

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.util.matching.Regex

class Day12 extends aoc.NewDay(2023, 12) {

  part(1) {
    test {
      """???.### 1,1,3
        |.??..??...?##. 1,1,3
        |?#?#?#?#?#?#?#? 1,3,1,6
        |????.#...#... 4,1,1
        |????.######..#####. 1,6,5
        |?###???????? 3,2,1""".stripMargin -> 21
    }

    execute { input =>
      input.map(validPermutations).sum
    }
  }

  part(2) {
    test {
      """???.### 1,1,3
        |.??..??...?##. 1,1,3
        |?#?#?#?#?#?#?#? 1,3,1,6
        |????.#...#... 4,1,1
        |????.######..#####. 1,6,5
        |?###???????? 3,2,1""".stripMargin -> 525152
    }

    execute { input =>
      input.map { line =>
        val Array(springs, runs) = line.split(' ')
        validPermutations(springs + "?" + springs + "?" + springs + "?" + springs + "?" + springs + " " + runs + "," + runs + "," + runs + "," + runs + "," + runs + ",")
      }.sum
    }
  }

  private[this] def validPermutations(line: String): Long = {
    val Array(springs, runs) = line.split(' ')
    validPermutations(springs, runs.split(',').map(_.toInt).toList)
  }

  // Thanks to https://github.com/sansskill for the implementation
  private[this] def validPermutations(springs: String, runs: List[Int]): Long = {
    val memo = (0 until springs.length).map { i =>
      springs.drop(i).takeWhile {
        c => c != '.'
      }.length
    }.toArray

    val dp = mutable.Map[(Int, Int), Long]()

    def canTake(i: Int, l: Int) = memo(i) >= l && (i + l == springs.length || springs(i + l) != '#')

    def helper(stringIndex: Int, runsIndex: Int): Long = {
      if (runsIndex == runs.size) {
        if (!springs.drop(stringIndex).exists {
          c => c == '#'
        }) 1L else 0
      } else if (stringIndex >= springs.length) {
        0L
      } else {
        if (!dp.contains((stringIndex, runsIndex))) {
          val take = if (canTake(stringIndex, runs(runsIndex))) helper(stringIndex + runs(runsIndex) + 1, runsIndex + 1) else 0L
          val dontTake = if (springs(stringIndex) != '#') helper(stringIndex + 1, runsIndex) else 0L
          dp((stringIndex, runsIndex)) = take + dontTake
        }
        dp((stringIndex, runsIndex))
      }
    }

    helper(0, 0)
  }
}

object Day12Main extends Day12

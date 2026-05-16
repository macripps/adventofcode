package aoc2021

import aoc.{NewDay, asGroupsSeparatedByBlankLines}

class Day4 extends NewDay(2021, 4) {
  import Day4._

  part(1) {
    execute { in =>
      val groups = asGroupsSeparatedByBlankLines(in)

      val callz = groups.head.head.split(',').map(_.toInt)

      val boards = groups.tail
      var k = boards.map { b =>
        Board(b.map { l =>
          val a = l.trim.split("[ ]+")
          val b = a.map(_.toInt)
          b
        }.toArray, Array(callz(0)))
      }

      var idx = 1
      while (!k.exists(b => b.rowOrColumnFilled)) {
        k = k.map { b =>
          b.copy(nums = b.nums, calls = b.calls :+ callz(idx))
        }
        idx = idx + 1
      }

      k.find(b => b.rowOrColumnFilled).head.score.toString
    }
  }

  part(2) {
    execute { in =>
      val groups = asGroupsSeparatedByBlankLines(in)

      val callz = groups.head.head.split(',').map(_.toInt)

      val boards = groups.tail
      var k = boards.map { b =>
        Board(b.map { l =>
          val a = l.trim.split("[ ]+")
          val b = a.map(_.toInt)
          b
        }.toArray, Array(callz(0)))
      }

      var idx = 1
      while (k.count(b => b.rowOrColumnFilled) != k.size - 1) {
        k = k.map { b =>
          b.copy(nums = b.nums, calls = b.calls :+ callz(idx))
        }
        idx = idx + 1
      }

      var notWonBoard = k.find(b => !b.rowOrColumnFilled).head

      while (!notWonBoard.rowOrColumnFilled) {
        notWonBoard = notWonBoard.copy(nums = notWonBoard.nums, calls = notWonBoard.calls :+ callz(idx))
        idx = idx + 1
      }

      notWonBoard.score.toString
    }
  }
}

object Day4 {
  case class Board(nums: Array[Array[Int]], calls: Array[Int]) {
    def rowOrColumnFilled: Boolean = {
      nums.exists(r => r.diff(calls).isEmpty) ||
        nums.transpose.exists(r => r.diff(calls).isEmpty)
    }

    def score: Int = {
      var t = nums.map(_.sum).sum
      calls.foreach { c =>
        if (nums.exists(a => a.contains(c))) {
          t = t - c
        }
      }
      t * calls.last
    }
  }
}

object Day4Main extends Day4

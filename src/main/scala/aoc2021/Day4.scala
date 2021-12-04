package aoc2021

import aoc.Day

class Day4 extends Day(2021, 4) {
  import Day4._

  override def part1(input: Array[String]): String = {
    val groups = inputGroups(input)

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

  override def part2(input: Array[String]): String = {
    val groups = inputGroups(input)

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

object Day4 {
  def apply() = new Day4()

  val example: Array[String] =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin.split("\n")

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

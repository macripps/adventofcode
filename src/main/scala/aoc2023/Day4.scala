package aoc2023

import scala.collection.mutable

class Day4 extends aoc.Day(2023, 4) {

  withPart1Test("""Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                  |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                  |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                  |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                  |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                  |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin, 13)
  override def part1(input: Array[String]): Any = {
    input.map { line =>
      val Array(card, nums) = line.split(':')
      val Array(win, have) = nums.split('|')
      val winningNums = win.trim.split(" +").map(_.toInt).toSet
      val haveNums = have.trim.split(" +").map(_.toInt).toSet
      (1 << (winningNums.intersect(haveNums).size)) / 2
    }.sum
  }

  withPart2Test("""Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                  |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                  |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                  |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                  |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                  |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin, 30)

  override def part2(input: Array[String]): Any = {
    val numCopies = mutable.Map[Int, Int]().withDefaultValue(0)
    input.foreach { line =>
      val Array(card, nums) = line.split(':')
      val cardNum = card.drop(5).trim.toInt
      numCopies(cardNum) += 1
      val Array(win, have) = nums.split('|')
      val winningNums = win.trim.split(" +").map(_.toInt).toSet
      val haveNums = have.trim.split(" +").map(_.toInt).toSet
      val copies = winningNums.intersect(haveNums).size
      (cardNum+1 to cardNum+copies).foreach { newCard =>
        numCopies(newCard) += numCopies(cardNum)
      }
    }
    numCopies.values.sum
  }
}

object Day4 {
  def apply() = new Day4
}

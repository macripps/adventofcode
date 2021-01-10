package aoc2016

import aoc.Day

class Day21 extends Day(2016, 21) {

  import Day21._

  val example = Seq(
    "swap position 4 with position 0",
    "swap letter d with letter b",
    "reverse positions 0 through 4",
    "rotate left 1 step",
    "move position 1 to position 4",
    "move position 3 to position 0",
    "rotate based on position of letter b",
    "rotate based on position of letter d",
  )

  override def part1: String = {
    val x = "abcdefgh".toCharArray
    input.foreach {
      case swapPosition(pos1: String, pos2: String) =>
        swapPosition(x, pos1.toInt, pos2.toInt)
      case swapLetter(l1: String, l2: String) =>
        swapLetter(x, l1.charAt(0), l2.charAt(0))
      case reversePositions(p1: String, p2: String) =>
        reversePositions(x, p1.toInt, p2.toInt)
      case rotateLeft(steps: String) =>
        rotateLeft(x, steps.toInt)
      case rotateRight(steps: String) =>
        rotateRight(x, steps.toInt)
      case movePosition(p1: String, p2: String) =>
        movePosition(x, p1.toInt, p2.toInt)
      case rotateLetter(p1: String) =>
        rotateLetter(x, p1.charAt(0))
    }
    x.mkString
  }

  def swapPosition(x: Array[Char], p1: Int, p2: Int): Unit = {
    val t = x(p1)
    x(p1) = x(p2)
    x(p2) = t
  }

  def swapLetter(x: Array[Char], c1: Char, c2: Char): Unit = {
    x.indices.foreach { i =>
      if (x(i) == c1) {
        x(i) = c2
      } else if (x(i) == c2) {
        x(i) = c1
      }
    }
  }

  def reversePositions(x: Array[Char], p1: Int, p2: Int): Unit = {
    val min = math.min(p1, p2)
    val max = math.max(p1, p2)
    (min to ((max + min) / 2)).foreach { p =>
      val t = x(p)
      x(p) = x(p2 - p + p1)
      x(p2 - p + p1) = t
    }
  }

  def rotateLeft(x: Array[Char], steps: Int): Unit = {
    val s = steps % x.length
    val z = x.take(s)
    Array.copy(x, s, x, 0, x.length - s)
    Array.copy(z, 0, x, x.length - s, s)
  }

  def rotateRight(x: Array[Char], steps: Int): Unit = {
    val s = steps % x.length
    val z = x.takeRight(s)
    Array.copy(x, 0, x, s, x.length - s)
    Array.copy(z, 0, x, 0, s)
  }

  def movePosition(x: Array[Char], p1: Int, p2: Int): Unit = {
    val t = x(p1)
    if (p2 > p1) {
      Array.copy(x, p1 + 1, x, p1, p2 - p1)
    } else {
      Array.copy(x, p2, x, p2 + 1, p1 - p2)
    }
    x(p2) = t
  }

  def rotateLetter(x: Array[Char], c: Char): Unit = {
    val i = x.indexOf(c)
    val r = i + 1 + (if (i >= 4) 1 else 0)
    rotateRight(x, r)
  }

  override def part2: String = {
    val x = "fbgdceah".toCharArray
    val unRotateLetter = Map(
      0 -> 7,
      1 -> 7,
      2 -> 2,
      3 -> 6,
      4 -> 1,
      5 -> 5,
      6 -> 0,
      7 -> 4,
    )
    input.reverse.foreach {
      case swapPosition(pos1: String, pos2: String) =>
        swapPosition(x, pos1.toInt, pos2.toInt)
      case swapLetter(l1: String, l2: String) =>
        swapLetter(x, l1.charAt(0), l2.charAt(0))
      case reversePositions(p1: String, p2: String) =>
        reversePositions(x, p1.toInt, p2.toInt)
      case rotateLeft(steps: String) =>
        rotateRight(x, steps.toInt)
      case rotateRight(steps: String) =>
        rotateLeft(x, steps.toInt)
      case movePosition(p1: String, p2: String) =>
        movePosition(x, p2.toInt, p1.toInt)
      case rotateLetter(p1: String) =>
        val r = unRotateLetter(x.indexOf(p1.charAt(0)))
        rotateRight(x, r)
    }
    x.mkString
  }
}

object Day21 {
  def apply() = new Day21()

  val swapPosition = raw"swap position (\d) with position (\d)".r
  val swapLetter = raw"swap letter (\w) with letter (\w)".r
  val rotateLeft = raw"rotate left (\d+) steps?".r
  val rotateRight = raw"rotate right (\d+) steps?".r
  val rotateLetter = raw"rotate based on position of letter (\w)".r
  val reversePositions = raw"reverse positions (\d) through (\d)".r
  val movePosition = raw"move position (\d) to position (\d)".r
}

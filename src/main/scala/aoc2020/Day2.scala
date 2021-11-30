package aoc2020

import aoc.Day
import aoc2020.Day2.{isValidPart1, isValidPart2}

class Day2 extends Day(2020, 2) {

  override def part1(input: Array[String]): String = {
    val valid = input.count(l => {
      val line = l.split(" ")
      val count = line(0)
      val minMax = count.split("-")
      val min = minMax(0).toInt
      val max = minMax(1).toInt
      val letter = line(1).charAt(0)
      val password = line(2)
      isValidPart1(password, letter, min, max)
    })
    "There are " + valid + " valid passwords in the file."
  }

  override def part2(input: Array[String]): String = {
    val valid = input.count(l => {
      val line = l.split(" ")
      val count = line(0)
      val minMax = count.split("-")
      val min = minMax(0).toInt
      val max = minMax(1).toInt
      val letter = line(1).charAt(0)
      val password = line(2)
      isValidPart2(password, letter, min, max)
    })
    "There are " + valid + " valid passwords in the file."
  }
}

object Day2 {
  def apply(): Day2 = {
    new Day2()
  }

  def isValidPart1(password: String, letter: Char, min: Int, max: Int): Boolean = {
    val letterCount = password.count(c => c == letter)
    letterCount >= min && letterCount <= max
  }

  def isValidPart2(password: String, letter: Char, min: Int, max: Int): Boolean = {
    (password.charAt(min - 1) == letter) ^ (password.charAt(max - 1) == letter)
  }
}

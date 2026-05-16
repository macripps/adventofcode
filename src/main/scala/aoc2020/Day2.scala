package aoc2020

import aoc.NewDay
import aoc2020.Day2.{isValidPart1, isValidPart2}

class Day2 extends NewDay(2020, 2) {

  part(1) {
    execute { in =>
      val valid = in.count(l => {
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
  }

  part(2) {
    execute { in =>
      val valid = in.count(l => {
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
}

object Day2Main extends Day2

object Day2 {
  def isValidPart1(password: String, letter: Char, min: Int, max: Int): Boolean = {
    val letterCount = password.count(c => c == letter)
    letterCount >= min && letterCount <= max
  }

  def isValidPart2(password: String, letter: Char, min: Int, max: Int): Boolean = {
    (password.charAt(min - 1) == letter) ^ (password.charAt(max - 1) == letter)
  }
}

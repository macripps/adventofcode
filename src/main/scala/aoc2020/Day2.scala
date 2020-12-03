package aoc2020

object Day2 {

  def main(): Unit = {
    val input = readFileToIterable("aoc2020/day2.input")
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
    println("There are " + valid + " valid passwords in the file.")
  }

  def isValidPart1(password: String, letter: Char, min: Int, max: Int): Boolean = {
    val letterCount = password.count(c => c == letter)
    letterCount >= min && letterCount <= max
  }

  def isValidPart2(password: String, letter: Char, min: Int, max: Int): Boolean = {
    (password.charAt(min - 1) == letter) ^ (password.charAt(max - 1) == letter)
  }
}

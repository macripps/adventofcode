package aoc2020

object Day5 {

  def main(): Unit = {
    val lines = readFileToIterable("aoc2020/day5.input")
    val seatIds = lines.map(toBinary).map(binaryToInt)

    println("The highest SeatID is " + seatIds.max)

    val sorted = seatIds.toArray.sorted
    val missing = sumFrom(sorted.head, sorted.last) - sorted.sum
    println("The missing SeatID is " + missing)
  }

  def toBinary(line: String): String = {
    line.replace('F', '0')
      .replace('B', '1')
      .replace('R', '1')
      .replace('L', '0')
  }

  def binaryToInt(binaryLine: String): Int = {
    Integer.parseInt(binaryLine, 2)
  }

  def sumFrom(low: Int, high: Int): Int = {
    (low + high) * (high + 1 - low) / 2
  }
}

package object aoc {

  trait Day {
    def year: Int
    def day: Int
    def part1(input: Array[String]): String
    def part2(input: Array[String]): String
  }

}
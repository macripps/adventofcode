package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  test("Minor example") {
    val lines = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                  |mem[8] = 11
                  |mem[7] = 101
                  |mem[8] = 0""".stripMargin.split("\n")

    assert(Day14().part1(lines) === "The sum of memory is 165")
  }

  test("Apply mask") {
    assert(Day14.applyValueMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 11) === 73)
    assert(Day14.applyValueMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 101) === 101)
    assert(Day14.applyValueMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 0) === 64)
//    assert(Day14.applyMask("000010X01101X0000101010X110010100000", 6131) === 0)
//    assert(Day14.applyValueMask("11X010X01101X100X0101101101001X01X1X", 6131) === 0)
  }
}

package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val testData = initTestData()

  def initTestData(): Array[Int] = {
    Array[Int](375,
      600,
      716,
      934,
      1100,
      1284,
      1399,
      1500,
      1999,
    )
  }

  test("finding the sum should work") {
    val result = Day1.findPairThatSumTo(testData, 2000)
    assert(result.isDefined)
    assert(result.get._1 == 716)
    assert(result.get._2 == 1284)
  }

  test("finding a non-existent sum should return None") {
    val result = Day1.findPairThatSumTo(testData, 1000)
    assert(result.isEmpty)
  }

}

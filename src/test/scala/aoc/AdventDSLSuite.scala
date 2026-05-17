package aoc

import org.scalatest.funsuite.AnyFunSuite

abstract class AdventDSLSuite(className: String) extends AnyFunSuite {
  private val day: AdventDSL =
    Class.forName(className).getConstructor().newInstance().asInstanceOf[AdventDSL]

  for {
    (part, cases) <- day.testCases.toSeq.sortBy(_._1)
    ((inputs, expected), idx) <- cases.zipWithIndex
  } {
    test(s"part $part case $idx") {
      assert(day.runTestCase(part, inputs) == expected)
    }
  }
}

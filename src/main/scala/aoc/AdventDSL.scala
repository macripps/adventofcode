package aoc

import scala.collection.mutable.ArrayBuffer


private[aoc] class PartDSL(part: Int) extends AdventDSL {
  require(part == 1 || part == 2, "Part must be 1 or 2")

  def apply(fn: => Unit): Unit = fn
}

private[aoc] trait AdventDSL { self =>
  private[aoc] val testCases = ArrayBuffer[(Array[String], Any)]()

  def part(part: Int): PartDSL = new PartDSL(part) {
    override private[aoc] val testCases = self.testCases
  }

  private[this] def addTest(testCase: (Array[String], Any)): Unit = {
    testCases += testCase
  }

  def test(t: => (String, Any)): Unit = {
    addTest(t._1.split("\n") -> t._2)
  }

  def execute(t: Array[String] => Any): Unit = {
//    setAlgorithm = t
  }

}

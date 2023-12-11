package aoc

import com.twitter.util.Var

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private trait AdventState {
  private[this] val defaultContext = AdventContext(currentPart = 0)

  private[aoc] val context: AdventContext = defaultContext

  private[aoc] lazy val contextVar = Var(defaultContext)
}

private[aoc] class PartDSL(part: Int) extends AdventDSL {
  require(part == 1 || part == 2, "Part must be 1 or 2")

  override private[aoc] val context = {
    val current = contextVar()
    current.copy(currentPart = part)
  }

  def apply(fn: => Unit): Unit = withContext(context)(fn)

  override private[aoc] def contextWrapper[T](f: => T): T = withContext(context) {
    f
  }
}

private[aoc] trait AdventDSL extends AdventState {
  self =>
  private[aoc] val testCases = mutable.Map[Int, ArrayBuffer[(Array[String], Any)]]()
  private[aoc] val executes = mutable.Map[Int, Array[String] => Any]()

  def part(part: Int): PartDSL = contextWrapper {
    new PartDSL(part) {
      override private[aoc] val testCases = self.testCases
      override private[aoc] val executes = self.executes
      override private[aoc] lazy val contextVar = self.contextVar
    }
  }

  private def addTest(testCase: (Array[String], Any)): Unit = {
    contextWrapper {
      if (testCases.contains(contextVar().currentPart)) {
        testCases(contextVar().currentPart).addOne(testCase)
      } else {
        testCases += (contextVar().currentPart -> ArrayBuffer(testCase))
      }
    }
  }

  private def addExecute(e: Array[String] => Any): Unit = {
    contextWrapper {
      executes(contextVar().currentPart) = e
    }
  }

  def test(t: => (String, Any)): Unit = {
    addTest(t._1.split("\n") -> t._2)
  }

  def execute(t: Array[String] => Any): Unit = {
    addExecute(t)
  }


  private[aoc] def contextWrapper[T](f: => T): T = {
    f
  }

  private[aoc] def withContext[T](ctx: AdventContext)(f: => T): T = {
    val orig = contextVar()
    contextVar() = ctx
    try {
      f
    } finally {
      contextVar() = orig
    }
  }

}

case class AdventContext(currentPart: Int)

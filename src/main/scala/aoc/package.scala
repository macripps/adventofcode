
import aoc.Direction.Direction
import io.opentelemetry.api.OpenTelemetry

import scala.annotation.tailrec
import scala.collection.Iterable.single
import scala.collection.{immutable, mutable}
import scala.io.Source

package object aoc {

  abstract class Day(val year: Int, val day: Int) {
    val tracer = OpenTelemetry.getGlobalTracer("aoc")

    lazy val input: Array[String] = {
      val inputSpan = tracer.spanBuilder("read_input").startSpan()
      val input = readFileToIterable("aoc" + year + "/day" + day + ".input").toArray
      inputSpan.`end`()
      input
    }

    lazy val inputGroups: Iterable[Iterable[String]] = asGroupsSeparatedByBlankLines(input)

    def part1: String
    def part2: String
  }

  def readFileToIterable(filename: String): Iterable[String] = {
    Source.fromResource(filename).getLines().to(Iterable)
  }

  def asGroupsSeparatedByBlankLines(lines: Array[String]): Iterable[Iterable[String]] = {
    val out = mutable.ListBuffer[List[String]]()
    val currentGroup = mutable.ListBuffer[String]()
    (lines ++ single("")).foreach { line =>
      if (line.isEmpty) {
        out += currentGroup.toList
        currentGroup.clear()
      } else {
        currentGroup += line
      }
    }
    out
  }

  case class Point(x: Int, y: Int) {
    def neighbours: List[Point] = {
      List(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1),
      )
    }

    def directionTo(other: Point): Direction = {
      if (other.x == this.x && other.y < this.y) {
        Direction.North
      } else if (other.x == this.x && other.y > this.y) {
        Direction.South
      } else if (other.x < this.x && other.y == this.y) {
        Direction.West
      } else if (other.x > this.x && other.y == this.y) {
        Direction.East
      } else {
        throw new IllegalArgumentException()
      }
    }

    val manhattanDistance: Int = math.abs(x) + math.abs(y)
  }

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West = Value
  }

  def breadthFirstSearch[A](root: A, transitionFunc: A => Set[A], isGoal: A => Boolean): A = {
    val seen = mutable.Set[A](root)
    val q = mutable.Queue(root)

    while (q.nonEmpty) {
      val v = q.dequeue()
      if (isGoal(v)) {
        return v
      }
      val ws = transitionFunc(v)
      ws.foreach { w =>
        if (!seen.contains(w)) {
          seen.addOne(w)
          q.addOne(w)
        }
      }
    }
    root
  }
}

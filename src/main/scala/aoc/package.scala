
import aoc.Direction.Direction
import io.opentelemetry.api.OpenTelemetry

import scala.collection.Iterable.single
import scala.collection.mutable
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

    def manhattanDistanceTo(p: Point): Int = math.abs(x - p.x) + math.abs(y - p.y)
  }

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West = Value
  }

  case class HexPoint(x: Int, y: Int, z: Int) {
    def s: HexPoint = copy(x = x + 1, y = y - 1)

    def n: HexPoint = copy(x = x - 1, y = y + 1)

    def sw: HexPoint = copy(x = x + 1, z = z - 1)

    def nw: HexPoint = copy(y = y + 1, z = z - 1)

    def se: HexPoint = copy(y = y - 1, z = z + 1)

    def ne: HexPoint = copy(x = x - 1, z = z + 1)

    def manhattanDistanceTo(p: HexPoint): Int = (math.abs(x - p.x) + math.abs(y - p.y) + math.abs(z - p.z)) / 2
  }

  def chineseRemainerTheorem(remainderModuliPairs: Array[(Int, Int)]): (Long, Long) = {
    var remainder: BigInt = remainderModuliPairs(0)._1
    var modulus: Long = remainderModuliPairs(0)._2
    remainderModuliPairs.drop(1).foreach { e =>
      val bz = bezoutIdentity(modulus, e._2)
      remainder = (remainder * e._2 * bz._2) + (modulus * e._1 * bz._1)
      modulus = modulus * e._2 / bz._3
      remainder = remainder % modulus
    }
    if (remainder < 0) {
      remainder = modulus + remainder
    }
    (remainder.longValue, modulus)
  }

  /**
   * Returns three numbers such that a * _1 + b * _2 = _3 (gcd(a,b))
   */
  def bezoutIdentity(a: Long, b: Long): (Long, Long, Long) = {
    var s0 = 1: Long
    var s1 = 0: Long
    var t0 = 0: Long
    var t1 = 1: Long
    var r0 = a
    var r1 = b
    while (r1 != 0) {
      val q = r0 / r1
      val r = r0 % r1
      val s = s0 - q * s1
      val t = t0 - q * t1
      s0 = s1
      s1 = s
      t0 = t1
      t1 = t
      r0 = r1
      r1 = r
    }
    (s0, t0, r0)
  }
}

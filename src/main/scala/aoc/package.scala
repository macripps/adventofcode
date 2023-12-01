
import aoc.Direction.Direction

import scala.collection.Iterable.single
import scala.collection.mutable
import scala.io.Source

package object aoc {

  abstract class Day(val year: Int, val day: Int) {
    var debug = false

    private[this] val part1Tests: mutable.Buffer[(String, Any)] = mutable.Buffer()
    private[this] val part2Tests: mutable.Buffer[(String, Any)] = mutable.Buffer()

    def runPart1Tests: Boolean = runTests(1, part1Tests, part1)
    def runPart2Tests: Boolean = runTests(2, part2Tests, part2)

    private[this] def runTests(part: Int, testInput: Iterable[(String, Any)], testMethod: Array[String] => Any): Boolean = {
      testInput.zipWithIndex.forall { e =>
        val result = testMethod(e._1._1.split('\n'))
        val testPass = result == e._1._2
        printf("%d.%d.%d.T%d: %s/%s: %s\n", year, day, part, e._2, result, e._1._2, if (testPass) {
          "✅"
        } else {
          "❌"
        })
        testPass
      }
    }

    protected def inputGroups(input: Array[String]): Iterable[Iterable[String]] = asGroupsSeparatedByBlankLines(input)

    def part1(input: Array[String]): Any

    def part2(input: Array[String]): Any

    protected def withPart1Test(input: String, expectedValue: Any) = part1Tests.append((input, expectedValue))

    protected def withPart2Test(input: String, expectedValue: Any) = part2Tests.append((input, expectedValue))
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
        Point(x, y - 1),
        Point(x - 1, y),
        Point(x + 1, y),
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

    override def toString: String = "(" + x + ", " + y + ")"
  }

  case class LongPoint(x: Long, y: Long) {
    def neighbours: List[LongPoint] = {
      List(
        LongPoint(x, y - 1),
        LongPoint(x - 1, y),
        LongPoint(x + 1, y),
        LongPoint(x, y + 1),
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

    def manhattanDistanceTo(p: Point): Long = math.abs(x - p.x) + math.abs(y - p.y)
  }

  case class ThreePoint(x: Double, y: Double, z: Double) {
    def manhattanDistanceTo(p: ThreePoint): Double = math.abs(x - p.x) + math.abs(y - p.y) + math.abs(z - p.z)
  }

  case class Point3(x: Int, y: Int, z: Int) {
    def neighbours: Set[Point3] = {
      Set(
        Point3(x, y - 1, z),
        Point3(x - 1, y, z),
        Point3(x + 1, y, z),
        Point3(x, y + 1, z),
        Point3(x, y, z - 1),
        Point3(x, y, z + 1),
      )
    }

    def manhattanDistanceTo(p: Point3): Int = math.abs(x - p.x) + math.abs(y - p.y) + math.abs(z - p.z)
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

  class ProgressBar(high: Int) {
    private[this] var amount: Int = 0

    private[this] var lastRendered: Int = 0

    def update(amt: Int): Unit = {
      amount = amt
    }

    def inc(): Unit = {
      amount = amount + 1
    }

    def delta(dlt: Int): Unit = {
      amount += dlt
    }

    private[this] val width = 40

    def render(): Unit = {
      val pct = 100 * amount / high
      if (pct != lastRendered) {
        print("[")
        print("=" * (40 * amount / high))
        print("-" * (40 - (40 * amount / high)))
        println("] " + pct + "% (" + amount + "/" + high + ")")
        lastRendered = pct
      }
    }
  }
}

package aoc2022

import scala.collection.mutable

class Day11 extends aoc.Day(2022, 11) {
  override def part1(input: Array[String]): Any = {
    val monkeys = inputMonkeys.map(m => m.id -> m).toMap
    var handled = Map[Int, Int]()

    (1 to 20).foreach { round =>
      (0 until monkeys.size).foreach { m =>
        val monkey = monkeys(m)
        while (monkey.items.nonEmpty) {
          var item = monkey.items.dequeue()
          handled = handled + (m -> (handled.getOrElse(m, 0) + 1))
          item = monkey.op(item)
          item = item / 3
          val divisible = (item % monkey.divisor) == 0
          val target = if (divisible) monkey.outputs._1 else monkey.outputs._2
          monkeys(target).items.enqueue(item)
        }
      }
    }
    handled.values.toSeq.sorted.takeRight(2).product
  }

  val inputMul = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

  private[this] def inputMonkeys = Seq(
    new Monkey(0, mutable.Queue(99, 63, 76, 93, 54, 73), x => (x * 11) % inputMul, 2, (7, 1)),
    new Monkey(1, mutable.Queue(91, 60, 97, 54), x => (x + 1) % inputMul, 17, (3, 2)),
    new Monkey(2, mutable.Queue(65), x => (x + 7) % inputMul, 7, (6, 5)),
    new Monkey(3, mutable.Queue(84, 55), x => (x + 3) % inputMul, 11, (2, 6)),
    new Monkey(4, mutable.Queue(86, 63, 79, 54, 83), x => (x * x) % inputMul, 19, (7, 0)),
    new Monkey(5, mutable.Queue(96, 67, 56, 95, 64, 69, 96), x => (x + 4) % inputMul, 5, (4, 0)),
    new Monkey(6, mutable.Queue(66, 94, 70, 93, 72, 67, 88, 51), x => (x * 5) % inputMul, 13, (4, 5)),
    new Monkey(7, mutable.Queue(59, 59, 74), x => (x + 8) % inputMul, 3, (1, 3)),
  )

  val testMul = 13 * 17 * 19 * 23

  private[this] def testMonkeys = Seq(
    new Monkey(0, mutable.Queue(79, 98), x => (x * 19) % testMul, 23, (2, 3)),
    new Monkey(1, mutable.Queue(54, 65, 75, 74), x => (x + 6) % testMul, 19, (2, 0)),
    new Monkey(2, mutable.Queue(79, 60, 97), x => (x * x) % testMul, 13, (1, 3)),
    new Monkey(3, mutable.Queue(74), x => (x + 3) % testMul, 17, (0, 1)),
  )

  override def part2(input: Array[String]): Any = {
    val monkeys = inputMonkeys.map(m => m.id -> m).toMap
    val interestingRounds = Seq(1, 20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)
    var handled = Map[Int, Long]()

    (1 to 10000).foreach { round =>
      (0 until monkeys.size).foreach { m =>
        val monkey = monkeys(m)
        while (monkey.items.nonEmpty) {
          var item = monkey.items.dequeue()
          handled = handled + (m -> (handled.getOrElse(m, 0L) + 1L))
          item = monkey.op(item)
          val divisible = (item % monkey.divisor) == 0
          val target = if (divisible) monkey.outputs._1 else monkey.outputs._2
          monkeys(target).items.enqueue(item)
        }
      }
    }
    handled.values.toSeq.sorted.takeRight(2).product
  }
}

object Day11 {
  def apply() = new Day11
}

class Monkey(val id: Int, val items: mutable.Queue[BigInt], val op: BigInt => BigInt, val divisor: Int, val outputs: (Int, Int)) {
}

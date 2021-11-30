package aoc2018

import aoc.Day

import scala.collection.mutable
import scala.util.matching.Regex

class Day16 extends Day(2018, 16) {

  import Day16._

  val example: Array[String] =
    """Before: [3, 2, 1, 1]
      |9 2 1 2
      |After:  [3, 2, 2, 1]
      |
      |
      |
      |9 2 1 2""".stripMargin.split("\n")

  override def part1(input: Array[String]): String = {
    inputGroups(input).dropRight(3).count { g =>
      var s: State = State(Array(0, 0, 0, 0))
      var i: Instruction = Instruction(0, 0, 0, 0)
      var o: State = State(Array(0, 0, 0, 0))
      g.foreach {
        case before(r0: String, r1: String, r2: String, r3: String) =>
          s = State(Array(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
        case instruction(op: String, a: String, b: String, c: String) =>
          i = Instruction(op.toInt, a.toInt, b.toInt, c.toInt)
        case after(r0: String, r1: String, r2: String, r3: String) =>
          o = State(Array(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
      }
      opcodes.count { p =>
        p.apply(s, i).reg sameElements o.reg
      } >= 3
    }.toString
  }

  override def part2(input: Array[String]): String = {
    val knownOps = mutable.Map[Int, OpCode]()
    val igs = inputGroups(input)
    while (knownOps.size != 16) {
      igs.dropRight(3).foreach { g =>
        var s: State = State(Array(0, 0, 0, 0))
        var i: Instruction = Instruction(0, 0, 0, 0)
        var o: State = State(Array(0, 0, 0, 0))
        g.foreach {
          case before(r0: String, r1: String, r2: String, r3: String) =>
            s = State(Array(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
          case instruction(op: String, a: String, b: String, c: String) =>
            i = Instruction(op.toInt, a.toInt, b.toInt, c.toInt)
          case after(r0: String, r1: String, r2: String, r3: String) =>
            o = State(Array(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
        }
        if (!knownOps.contains(i.opCode)) {
          val n = opcodes.filter { p =>
            !knownOps.values.toSet.contains(p) && p.apply(s, i).reg.sameElements(o.reg)
          }
          if (n.size == 1) {
            knownOps.addOne(i.opCode, n.head)
          }
        }
      }
    }
    if (debug) {
      println(knownOps)
    }
    var s = State(Array(0, 0, 0, 0))
    igs.takeRight(1).foreach { g =>
      g.foreach {
        case instruction(op: String, a: String, b: String, c: String) =>
          s = knownOps(op.toInt)(s, Instruction(op.toInt, a.toInt, b.toInt, c.toInt))
      }
    }
    s.reg(0).toString
  }
}

object Day16 {
  def apply() = new Day16()

  case class State(reg: Array[Int]) {
    override def toString: String = "State(" + reg.mkString(",") + ")"
  }

  case class Instruction(opCode: Int, a: Int, b: Int, c: Int)

  val opcodes: Seq[OpCode] = Seq[OpCode](
    addr,
    addi,
    mulr,
    muli,
    banr,
    bani,
    borr,
    bori,
    setr,
    seti,
    gtir,
    gtri,
    gtrr,
    eqir,
    eqri,
    eqrr,
  )

  trait OpCode {
    def apply(s: State, i: Instruction): State
  }

  case object addr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) + n(i.b)
      State(n)
    }
  }

  case object addi extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) + i.b
      State(n)
    }
  }

  case object mulr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) * n(i.b)
      State(n)
    }
  }

  case object muli extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) * i.b
      State(n)
    }
  }

  case object banr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) & n(i.b)
      State(n)
    }
  }

  case object bani extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) & i.b
      State(n)
    }
  }

  case object borr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) | n(i.b)
      State(n)
    }
  }

  case object bori extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a) | i.b
      State(n)
    }
  }

  case object setr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = n(i.a)
      State(n)
    }
  }

  case object seti extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = i.a
      State(n)
    }
  }

  case object gtir extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = if (i.a > n(i.b)) 1 else 0
      State(n)
    }
  }

  case object gtri extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = if (n(i.a) > i.b) 1 else 0
      State(n)
    }
  }

  case object gtrr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = if (n(i.a) > n(i.b)) 1 else 0
      State(n)
    }
  }

  case object eqir extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = if (i.a == n(i.b)) 1 else 0
      State(n)
    }
  }

  case object eqri extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = if (n(i.a) == i.b) 1 else 0
      State(n)
    }
  }

  case object eqrr extends OpCode {
    override def apply(s: State, i: Instruction): State = {
      val n = s.reg.clone()
      n(i.c) = if (n(i.a) == n(i.b)) 1 else 0
      State(n)
    }
  }

  val before: Regex = raw"Before: \[(\d), (\d), (\d), (\d)\]".r
  val instruction: Regex = raw"(\d+) (\d) (\d) (\d)".r
  val after: Regex = raw"After:  \[(\d), (\d), (\d), (\d)\]".r
}

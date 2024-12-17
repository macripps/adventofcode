package aoc2024

import aoc.NewDay

import scala.collection.mutable

class Day17 extends NewDay(2024, 17) {
  part(1) {
    test(
      """Register A: 729
        |Register B: 0
        |Register C: 0
        |
        |Program: 0,1,5,4,3,0""".stripMargin -> "4,6,3,5,6,3,5,2,1,0")
    execute { ls =>
      val regA = ls(0).split(": ")(1).toLong
      val regB = ls(1).split(": ")(1).toLong
      val regC = ls(2).split(": ")(1).toLong

      val program = ls(4).split(": ")(1).split(",").map(_.toInt)
      run(program, regA, regB, regC)
    }
  }

  private[this] def run(program: Array[Int], a: Long, b: Long, c: Long): String = {
    var regA = a
    var regB = b
    var regC = c
    val output = mutable.ListBuffer[String]()
    var ip = 0
    while (ip < program.length) {
      val opCode = program(ip)
      val operand = program(ip + 1)
      opCode match {
        case 0 => // adv
          regA = regA / (1 << combo(operand, regA, regB, regC))
          ip = ip + 2
        case 1 => // bxl
          regB = regB ^ operand
          ip = ip + 2
        case 2 => // bst
          regB = combo(operand, regA, regB, regC) & 7
          ip = ip + 2
        case 3 => // jnz
          if (regA != 0) {
            ip = operand
          } else {
            ip = ip + 2
          }
        case 4 => // bxc
          regB = regB ^ regC
          ip = ip + 2
        case 5 => // out
          output += (combo(operand, regA, regB, regC) & 7).toString
          ip = ip + 2
        case 6 => // bdv
          regB = regA / (1 << combo(operand, regA, regB, regC))
          ip = ip + 2
        case 7 => // cdv
          regC = regA / (1 << combo(operand, regA, regB, regC))
          ip = ip + 2
      }
    }
    output.mkString(",")
  }

  part(2) {
    execute { ls =>
      val programStr = ls(4).split(": ")(1)
      val program = programStr.split(",").map(_.toInt)
      search(0L, 1, program)
      mins.min
    }
  }

  private[this] val mins = mutable.Set[Long]()

  private[this] def search(a: Long, inst: Int, program: Array[Int]): Long = {
    (0L to 7L).foreach { v =>
      val t = a | v
      if (exec(t).sameElements(program.drop(16 - inst))) {
        if (inst == 16) {
          mins += t
        } else {
          search(t << 3, inst + 1, program)
        }
      }
    }
    a
  }

  private[this] def exec(a: Long): List[Int] = {
    val out = mutable.ListBuffer[Int]()
    var v = a
    do {
      var regB = v & 7
      regB = regB ^ 7
      val regC = v >> regB
      regB = regB ^ 7
      v = v >> 3
      regB = regB ^ regC
      out += (regB & 7).toInt
    } while (v != 0)
    out.toList
  }

  private[this] def combo(i: Int, rA: Long, rB: Long, rC: Long): Long = {
    i match {
      case 0 | 1 | 2 | 3 => i
      case 4 => rA
      case 5 => rB
      case 6 => rC
    }
  }
}

object Day17Main extends Day17

package aoc2015

import aoc.Day
import Day23._

class Day23 extends Day(2015, 23) {
  override def part1: String = {
    val c = new Computer()
    c.execute(input)
    c.regB.toString()
  }

  override def part2: String = {
    val c = new Computer()
    c.regA = 1
    c.execute(input)
    c.regB.toString()
  }
}

object Day23 {
  def apply() = new Day23()

  class Computer {
    var regA: BigInt = 0
    var regB: BigInt = 0

    def execute(input: Array[String]): Unit = {
      var idx = 0
      while (idx >= 0 && idx < input.length) {
        input(idx) match {
          case Half(reg) => {
            if (reg == "a") {
              regA = regA / 2
            } else {
              regB = regB / 2
            }
            idx = idx + 1
          }
          case Triple(reg) => {
            if (reg == "a") {
              regA = regA * 3
            } else {
              regB = regB * 3
            }
            idx = idx + 1
          }
          case Increment(reg) => {
            if (reg == "a") {
              regA = regA + 1
            } else {
              regB = regB + 1
            }
            idx = idx + 1
          }
          case Jump(offset) => {
            idx = idx + offset.toInt
          }
          case JumpIfEven(reg, offset) => {
            if (reg == "a") {
              if (regA % 2 == 0) {
                idx = idx + offset.toInt
              } else {
                idx = idx + 1
              }
            } else {
              if (regB % 2 == 0) {
                idx = idx + offset.toInt
              } else {
                idx = idx + 1
              }
            }
          }
          case JumpIfOne(reg, offset) => {
            if (reg == "a") {
              if (regA == 1) {
                idx = idx + offset.toInt
              } else {
                idx = idx + 1
              }
            } else {
              if (regB == 1) {
                idx = idx + offset.toInt
              } else {
                idx = idx + 1
              }
            }
          }
        }
      }
    }
  }

  val Half = raw"hlf (\w)".r
  val Triple = raw"tpl (\w)".r
  val Increment = raw"inc (\w)".r
  val Jump = raw"jmp \+?(-?\d+)".r
  val JumpIfEven = raw"jie (\w), \+?(-?\d+)".r
  val JumpIfOne = raw"jio (\w), \+?(-?\d+)".r
}

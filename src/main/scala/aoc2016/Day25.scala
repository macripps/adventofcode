package aoc2016

import aoc.Day

import scala.collection.mutable

class Day25 extends Day(2016, 25){
  import Day12._
  import Day23._
  import Day25._

  override def part1: String = {
    LazyList.from(1).find { a =>
      println(a)
      val registers = mutable.Map("a" -> a.toLong, "b" -> 0L, "c" -> 0L, "d" -> 0L)
      execute(input.toBuffer, registers)
    }.toString
  }

  override def part2: String = ""

  def execute(program: mutable.Buffer[String], registers: mutable.Map[String, Long]): Boolean = {
    var ep = 0
    var validOutput = true
    var expectedOutput = 0L
    var outputLength = 0
    while (ep < program.length && validOutput && outputLength < 500) {
      program(ep) match {
        case cpy(amount: String, reg: String) =>
          if (reg == "a" || reg == "b" || reg == "c" || reg == "d") {
            if (amount == "a" || amount == "b" || amount == "c" || amount == "d") {
              registers(reg) = registers(amount)
            } else {
              registers(reg) = amount.toInt
            }
          }
          ep = ep + 1
        case inc(reg: String) =>
          if (reg == "a" || reg == "b" || reg == "c" || reg == "d") {
            registers(reg) = registers(reg) + 1
          }
          ep = ep + 1
        case dec(reg: String) =>
          if (reg == "a" || reg == "b" || reg == "c" || reg == "d") {
            registers(reg) = registers(reg) - 1
          }
          ep = ep + 1
        case jnz(reg: String, offset: String) =>
          val off = if (offset == "a" || offset == "b" || offset =="c" || offset == "d") {
            registers(offset)
          } else {
            offset.toInt
          }
          if (reg == "a" || reg == "b" || reg == "c" || reg == "d") {
            if (registers(reg) != 0) {
              ep = ep + off.toInt
            } else {
              ep = ep + 1
            }
          } else if (reg.toInt != 0) {
            ep = ep + off.toInt
          } else {
            ep = ep + 1
          }
        case tgl(reg: String) =>
          val offset = registers(reg)
          val dest = ep + offset.toInt
          if (dest < 0 || dest >= input.length) {
            // Do nothing
          } else {
            val inst = program(dest)
            val newInst = inst.take(3) match {
              case "inc" => "dec"
              case "dec" => "inc"
              case "tgl" => "inc"
              case "jnz" => "cpy"
              case "cpy" => "jnz"
              case _ => ""
            }
            program(dest) = newInst + inst.drop(3)
          }
          ep = ep + 1
        case out(reg: String) =>
          print(registers(reg))
          if (registers(reg) != expectedOutput) {
            validOutput = false
          }
          outputLength = outputLength + 1
          expectedOutput = 1L - expectedOutput
          ep = ep + 1
      }
    }
    println()
    validOutput
  }
}

object Day25 {
  def apply() = new Day25()

  val out = raw"out (\w)".r
}

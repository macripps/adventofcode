package aoc2016

import aoc.Day

import scala.collection.mutable

class Day23 extends Day(2016, 23) {
  import Day12._
  import Day23._

  override def part1: String = {
    val registers = mutable.Map("a" -> 7L, "b" -> 0L, "c" -> 0L, "d" -> 0L)
    execute(input.toBuffer, registers)
    registers("a").toString
  }

  override def part2: String = {
    val registers = mutable.Map("a" -> 12L, "b" -> 0L, "c" -> 0L, "d" -> 0L)
    execute(input.toBuffer, registers)
    registers("a").toString
  }

  def execute(program: mutable.Buffer[String], registers: mutable.Map[String, Long]): Unit = {
    var ep = 0
    while (ep < program.length) {
      if (ep == 10 || ep == 16) {
        println(ep + ": " + program(ep) + ": " + registers)
      }
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
          println("toggling " + dest)
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
      }
    }
  }
}

object Day23 {
  def apply() = new Day23()

  val tgl = raw"tgl (\w)".r
}

/*
cpy a b       var b = a
  dec b         b = b - 1
  cpy a d       var d = a
  cpy 0 a       a = 0
    cpy b c       var c = b
      inc a
      dec c
      jnz c -2      a = c, c = 0
    dec d         d = d - 1
    jnz d -5      a = c * d, c = 0, d = 0 => a = (b-1) * a
  dec b         b = b - 1
  cpy b c       c = b
  cpy c d       d = c
    dec d
    inc c
    jnz d -2      c = c * 2, d = 0 => c = (initial-input -2) * 2, a = (initial-input-1)*(initial-input)
  tgl c // toggle to inc
  cpy -16 c
  jnz 1 c     a = (initial-input!) // toggle to copy
cpy 71 c
jnz 72 d // toggle to cpy
inc a
inc d // toggle to dec
jnz d -2
inc c // toggle to dec
jnz c -5     a = (initial-input! + (71*72))

 */

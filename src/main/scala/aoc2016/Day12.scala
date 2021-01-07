package aoc2016

import aoc.Day
import aoc2016.Day12._

import scala.collection.mutable

class Day12 extends Day(2016, 12) {
  override def part1: String = {
    var ep = 0
    val registers = mutable.Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
    while (ep < input.length) {
      input(ep) match {
        case cpy(amount: String, reg: String) =>
          if (amount == "a" || amount == "b" || amount == "c" || amount == "d") {
            registers(reg) = registers(amount)
          } else {
            registers(reg) = amount.toInt
          }
          ep = ep + 1
        case inc(reg: String) =>
          registers(reg) = registers(reg) + 1
          ep = ep + 1
        case dec(reg: String) =>
          registers(reg) = registers(reg) - 1
          ep = ep + 1
        case jnz(reg: String, offset: String) =>
          if (reg == "a" || reg == "b" || reg == "c" || reg == "d") {
            if (registers(reg) != 0) {
              ep = ep + offset.toInt
            } else {
              ep = ep + 1
            }
          } else if (reg.toInt != 0) {
            ep = ep + offset.toInt
          } else {
            ep = ep + 1
          }
      }
    }
    registers("a").toString
  }

  override def part2: String = {

    var ep = 0
    val registers = mutable.Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)
    while (ep < input.length) {
      input(ep) match {
        case cpy(amount: String, reg: String) =>
          if (amount == "a" || amount == "b" || amount == "c" || amount == "d") {
            registers(reg) = registers(amount)
          } else {
            registers(reg) = amount.toInt
          }
          ep = ep + 1
        case inc(reg: String) =>
          registers(reg) = registers(reg) + 1
          ep = ep + 1
        case dec(reg: String) =>
          registers(reg) = registers(reg) - 1
          ep = ep + 1
        case jnz(reg: String, offset: String) =>
          if (reg == "a" || reg == "b" || reg == "c" || reg == "d") {
            if (registers(reg) != 0) {
              ep = ep + offset.toInt
            } else {
              ep = ep + 1
            }
          } else if (reg.toInt != 0) {
            ep = ep + offset.toInt
          } else {
            ep = ep + 1
          }
      }
    }
    registers("a").toString
  }
}

object Day12 {
  def apply() = new Day12()

  val cpy = raw"cpy (.+?) (.)".r
  val inc = raw"inc (.)".r
  val dec = raw"dec (.)".r
  val jnz = raw"jnz (.) (.+)".r
}

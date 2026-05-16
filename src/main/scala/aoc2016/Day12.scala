package aoc2016

import aoc.NewDay
import aoc2016.Day12._

import scala.collection.mutable
import scala.util.matching.Regex

class Day12 extends NewDay(2016, 12) {
  part(1) {
    execute { in =>
      var ep = 0
      val registers = mutable.Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
      while (ep < in.length) {
        in(ep) match {
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

  part(2) {
    execute { in =>
      var ep = 0
      val registers = mutable.Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)
      while (ep < in.length) {
        in(ep) match {
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
}

object Day12 {
  val cpy: Regex = raw"cpy (.+?) (.)".r
  val inc: Regex = raw"inc (.)".r
  val dec: Regex = raw"dec (.)".r
  val jnz: Regex = raw"jnz (.) (.+)".r
}

object Day12Main extends Day12

package aoc2017

import aoc.NewDay

import scala.collection.mutable
import scala.util.matching.Regex

class Day23 extends NewDay(2017, 23) {

  import Day23._

  part(1) {
    execute { in =>
      val registers = mutable.Map[String, Long]()
      var ep = 0
      var muls = 0
      while (ep >= 0 && ep < in.length) {
        in(ep) match {
          case set(r1: String, rOrVal: String) =>
            registers(r1) = regOrVal(registers, rOrVal)
            ep = ep + 1
          case sub(r1: String, rOrVal: String) =>
            registers(r1) = regOrVal(registers, r1) - regOrVal(registers, rOrVal)
            ep = ep + 1
          case mul(r1: String, rOrVal: String) =>
            muls = muls + 1
            registers(r1) = regOrVal(registers, r1) * regOrVal(registers, rOrVal)
            ep = ep + 1
          case jnz(rOrVal1: String, rOrVal2: String) =>
            val x = regOrVal(registers, rOrVal1)
            if (x != 0) {
              ep = ep + regOrVal(registers, rOrVal2).toInt
            } else {
              ep = ep + 1
            }
        }
      }
      println(registers)
      muls.toString
    }
  }

  def regOrVal(registers: mutable.Map[String, Long], str: String): Long = {
    if (str.charAt(0) >= 'a' && str.charAt(0) <= 'z') {
      registers.getOrElse(str, 0)
    } else str.toLong
  }

  part(2) {
    execute { _ =>
      // Setup
      var b = 0
      var c = 0
      // Init
      b = 99
      c = b
      b = b * 100
      b = b + 100_000
      c = b
      c = c + 17_000
      Range.inclusive(b, c, 17).count(!isPrime(_)).toString
    }
  }

  def isPrime(x: Long): Boolean = {
    (2 to math.sqrt(x.toDouble).toInt).foreach { i =>
      if (x % i == 0) {
        return false
      }
    }
    true
  }
}

object Day23 {
  val set: Regex = raw"set (\w) (-?\w+)".r
  val sub: Regex = raw"sub (\w) (-?\w+)".r
  val mul: Regex = raw"mul (\w) (-?\w+)".r
  val jnz: Regex = raw"jnz (-?\w+) (-?\w+)".r
}

object Day23Main extends Day23

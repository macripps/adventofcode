package aoc2017

import aoc.Day

import scala.collection.mutable

class Day8 extends Day(2017, 8) {
  override def part1(input: Array[String]): String = {
    val registers = mutable.Map[String, Int]()
    input.foreach { line =>
      val cmd = line.split(" ")
      val r1 = cmd(0)
      val op = cmd(1)
      val dlt = cmd(2).toInt * (if (op == "inc") 1 else -1)
      val r2 = cmd(4)
      val cmp = cmd(5)
      val amt = cmd(6)
      cmp match {
        case "<" => if (registers.getOrElse(r2, 0) < amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case ">" => if (registers.getOrElse(r2, 0) > amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case "!=" => if (registers.getOrElse(r2, 0) != amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case "==" => if (registers.getOrElse(r2, 0) == amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case "<=" => if (registers.getOrElse(r2, 0) <= amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case ">=" => if (registers.getOrElse(r2, 0) >= amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
      }
    }
    registers.maxBy(x => x._2)._2.toString
  }

  override def part2(input: Array[String]): String = {
    val registers = mutable.Map[String, Int]()
    var mx = Int.MinValue
    input.foreach { line =>
      val cmd = line.split(" ")
      val r1 = cmd(0)
      val op = cmd(1)
      val dlt = cmd(2).toInt * (if (op == "inc") 1 else -1)
      val r2 = cmd(4)
      val cmp = cmd(5)
      val amt = cmd(6)
      cmp match {
        case "<" => if (registers.getOrElse(r2, 0) < amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case ">" => if (registers.getOrElse(r2, 0) > amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case "!=" => if (registers.getOrElse(r2, 0) != amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case "==" => if (registers.getOrElse(r2, 0) == amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case "<=" => if (registers.getOrElse(r2, 0) <= amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
        case ">=" => if (registers.getOrElse(r2, 0) >= amt.toInt) { registers(r1) = registers.getOrElse(r1, 0) + dlt }
      }
      if (registers.getOrElse(r1, 0) > mx) {
        mx = registers.getOrElse(r1, 0)
      }
    }
    mx.toString
  }
}

object Day8 {
  def apply() = new Day8()
}

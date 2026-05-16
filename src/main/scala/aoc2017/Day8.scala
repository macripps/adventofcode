package aoc2017

import aoc.NewDay

import scala.collection.mutable

class Day8 extends NewDay(2017, 8) {
  part(1) {
    execute { in =>
      val registers = mutable.Map[String, Int]()
      in.foreach { line =>
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
  }

  part(2) {
    execute { in =>
      val registers = mutable.Map[String, Int]()
      var mx = Int.MinValue
      in.foreach { line =>
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
}

object Day8Main extends Day8

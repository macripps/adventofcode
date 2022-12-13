package aoc2019

import aoc.Day
import com.twitter.concurrent.Broker
import com.twitter.util.Await

import scala.util.control.Breaks.breakable

class Day2 extends Day(2019, 2) {
  override def part1(input: Array[String]): String = {
    val program = input.head.split(',').map(_.toInt)
    program(1) = 12
    program(2) = 2
    val ic = new IntCode(program)
    val in = new Broker[Int]
    val out = new Broker[Int]
    Await.result(ic.execute(in, out))
    program(0).toString
  }

  override def part2(input: Array[String]): String = {
    (1 to 100).foreach { noun =>
      (1 to 100).foreach { verb =>
        val program = input.head.split(',').map(_.toInt)
        program(1) = noun
        program(2) = verb
        val ic = new IntCode(program)
        val in = new Broker[Int]
        val out = new Broker[Int]
        Await.result(ic.execute(in, out))
        if (program(0) == 19690720) {
          println(noun, verb)
        }
      }
    }
    ""
  }
}

object Day2 {
  def apply() = new Day2()
}

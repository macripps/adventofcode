package aoc2019

import aoc.Day
import com.twitter.concurrent.Broker
import com.twitter.util.Await

import scala.collection.mutable

class Day5 extends Day(2019, 5) {
  override def part1(input: Array[String]): Any = {
    val program = input.head.split(',').map(_.toInt)
    val ic = new IntCode(program)
    val in = new Broker[Int]()
    val out = new Broker[Int]()
    in ! 1
    val exec = ic.execute(in, out)
    while (!exec.isDefined) {
      println(Await.result(out.recv.sync()))
    }
  }

  override def part2(input: Array[String]): Any = {
    val program = input.head.split(',').map(_.toInt)
    val ic = new IntCode(program)
    val in = new Broker[Int]()
    val out = new Broker[Int]()
    in ! 5
    val exec = ic.execute(in, out)
    while (!exec.isDefined) {
      println(Await.result(out.recv.sync()))
    }
  }
}

object Day5 {
  def apply() = new Day5()
}

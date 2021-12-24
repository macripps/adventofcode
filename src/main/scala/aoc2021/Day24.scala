package aoc2021

import aoc.Day
import aoc2021.Day24.executeNative

import scala.collection.mutable

class Day24 extends Day(2021, 24) {
  override def part1(input: Array[String]): String = {
    var in: Long = 100000000000000L
    var running = true
    var smallest = Long.MaxValue
    while (running) {
      in = in - 1
      val inputs = in.toString.toCharArray.map(_ - '0').toSeq
      if (!inputs.contains(0)) {
        //        val t0 = System.nanoTime()
        //        val (_, _, _, z1) = execute(input, inputs)
        val t1 = System.nanoTime()
        val (_, _, _, z2) = executeNative(inputs)
        val t2 = System.nanoTime()
        //        println("One interpreted execution took " + (t1 - t0) + " nanos with result " + z1)
        if (z2 < smallest) {
          println("One native execution took " + (t2 - t1) + " nanos with result " + z2)
          smallest = z2
          println(in, z2)
        }
        running = (z2 != 0)
      }
    }
    in.toString
  }

  def execute(program: Array[String], input: Seq[Int]): (Long, Long, Long, Long) = {
    val reg: mutable.Map[Char, Long] = mutable.Map('w' -> 0L, 'x' -> 0L, 'y' -> 0L, 'z' -> 0L)
    var i = 0
    program.foreach { l =>
      val cmd = l.split(" ")
      cmd(0) match {
        case "inp" =>
          reg(cmd(1).charAt(0)) = input(i)
          i = i + 1
        case "mul" =>
          val o = cmd(2).charAt(0)
          if (o == 'w' || o == 'x' || o == 'y' || o == 'z') {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) * reg(o)
          } else {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) * cmd(2).toLong
          }
        case "add" =>
          val o = cmd(2).charAt(0)
          if (o == 'w' || o == 'x' || o == 'y' || o == 'z') {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) + reg(o)
          } else {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) + cmd(2).toLong
          }
        case "mod" =>
          val o = cmd(2).charAt(0)
          if (o == 'w' || o == 'x' || o == 'y' || o == 'z') {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) % reg(o)
          } else {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) % cmd(2).toLong
          }
        case "div" =>
          val o = cmd(2).charAt(0)
          if (o == 'w' || o == 'x' || o == 'y' || o == 'z') {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) / reg(o)
          } else {
            reg(cmd(1).charAt(0)) = reg(cmd(1).charAt(0)) / cmd(2).toLong
          }
        case "eql" =>
          val o = cmd(2).charAt(0)
          if (o == 'w' || o == 'x' || o == 'y' || o == 'z') {
            reg(cmd(1).charAt(0)) = if (reg(cmd(1).charAt(0)) == reg(o)) 1 else 0
          } else {
            reg(cmd(1).charAt(0)) = if (reg(cmd(1).charAt(0)) == cmd(2).toLong) 1 else 0
          }
        case _ =>
          println("Unknown command " + cmd(0))
      }

    }
    (reg('w'), reg('x'), reg('y'), reg('z'))
  }

  override def part2(input: Array[String]): String = {
    ""
  }
}

object Day24 {
  def apply() = new Day24

  def executeNative(input: Seq[Int]): (Long, Long, Long, Long) = {
    var w = 0L
    var x = 0L
    var y = 0L
    var z = 0L
    w = input(0)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 14
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = w
    y = y + 16
    y = y * x
    z = z + y
    w = input(1)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 11
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 25
    y = y * x
    y = y + 1
    z = z * y
    y = w
    y = y + 3
    y = y * x
    z = z + y
    w = input(2)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 12
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 2
    y = y * x
    z = z + y
    w = input(3)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 11
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 7
    y = y * x
    z = z + y
    w = input(4)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 10
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 13
    y = y * x
    z = z + y
    w = input(5)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 15
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 6
    y = y * x
    z = z + y
    w = input(6)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 14
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 10
    y = y * x
    z = z + y
    w = input(7)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 10
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 11
    y = y * x
    z = z + y
    w = input(8)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 6
    y = y * x
    z = z + y
    w = input(9)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 3
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 5
    y = y * x
    z = z + y
    w = input(10)
    x = 0
    x = x + z
    x = x % 26
    z = z / 1
    x = x + 13
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 11
    y = y * x
    z = z + y
    w = input(11)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 3
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 4
    y = y * x
    z = z + y
    w = input(12)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 9
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 4
    y = y * x
    z = z + y
    w = input(13)
    x = 0
    x = x + z
    x = x % 26
    z = z / 26
    x = x - 12
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + 6
    y = y * x
    z = z + y
    (w, x, y, z)
  }
}

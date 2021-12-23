package aoc2019

import scala.collection.mutable

class IntCode(program: Array[Int]) {

  def execute(in: mutable.Queue[Int] = mutable.Queue[Int]()): Unit = {
    var ip = 0
    while (program(ip) != 99) {
      program(ip) match {
        case 1 => // ADD
          program(program(ip + 3)) = program(program(ip + 1)) + program(program(ip + 2))
          ip = ip + 4
        case 2 => // MUL
          program(program(ip + 3)) = program(program(ip + 1)) * program(program(ip + 2))
          ip = ip + 4
        case 3 => // IN
          program(program(ip+1)) = in.dequeue()
          ip = ip + 2
        case 4 => // OUT
          println(program(ip+1))
          ip = ip + 2
      }
    }
  }
}

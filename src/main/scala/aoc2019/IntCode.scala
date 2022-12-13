package aoc2019

import com.twitter.concurrent.Broker
import com.twitter.util.{Await, Future}
import com.twitter.util.FuturePool.unboundedPool

class IntCode(program: Array[Int]) {

  def execute(in: Broker[Int], out: Broker[Int]): Future[Unit] = {
    unboundedPool {
      var running_ = true
      var ip = 0
      while (running_) {
        val opcode = program(ip) % 100
        val modes = program(ip) / 100
        val op1mode = modes % 10
        val op2mode = (modes / 10) % 10
        val op3mode = (modes / 100) % 10
        opcode match {
          case 1 => // ADD
            program(program(ip + 3)) = mode(op1mode, program(ip + 1)) + mode(op2mode, program(ip + 2))
            ip = ip + 4
          case 2 => // MUL
            program(program(ip + 3)) = mode(op1mode, program(ip + 1)) * mode(op2mode, program(ip + 2))
            ip = ip + 4
          case 3 => // IN
            val n = in.??
            program(program(ip + 1)) = n
            ip = ip + 2
          case 4 => // OUT
            val n = mode(op1mode, program(ip + 1))
            out !! n
            ip = ip + 2
          case 5 =>
            if (mode(op1mode, program(ip + 1)) != 0) {
              ip = mode(op2mode, program(ip + 2))
            } else {
              ip = ip + 3
            }
          case 6 =>
            if (mode(op1mode, program(ip + 1)) == 0) {
              ip = mode(op2mode, program(ip + 2))
            } else {
              ip = ip + 3
            }
          case 7 =>
            if (mode(op1mode, program(ip + 1)) < mode(op2mode, program(ip + 2))) {
              program(program(ip + 3)) = 1
            } else {
              program(program(ip + 3)) = 0
            }
            ip = ip + 4
          case 8 =>
            if (mode(op1mode, program(ip + 1)) == mode(op2mode, program(ip + 2))) {
              program(program(ip + 3)) = 1
            } else {
              program(program(ip + 3)) = 0
            }
            ip = ip + 4
          case 99 => running_ = false
        }
      }
    }
  }

  def mode(p: Int, value: Int): Int = {
    if (p == 0) program(value)
    else if (p == 1) value
    else Integer.MIN_VALUE
  }
}

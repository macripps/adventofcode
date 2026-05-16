package aoc2019

import aoc.NewDay
import com.twitter.concurrent.Broker
import com.twitter.util.Await

import scala.collection.mutable

class Day5 extends NewDay(2019, 5) {
  part(1) {
    execute { in =>
      val program = in.head.split(',').map(_.toInt)
      val ic = new IntCode(program)
      val input = new Broker[Int]()
      val output = new Broker[Int]()
      input ! 1
      val exec = ic.execute(input, output)
      val outputs = mutable.Buffer[Int]()
      while (!exec.isDefined) {
        outputs += Await.result(output.recv.sync())
      }
      outputs.last
    }
  }

  part(2) {
    execute { in =>
      val program = in.head.split(',').map(_.toInt)
      val ic = new IntCode(program)
      val input = new Broker[Int]()
      val output = new Broker[Int]()
      input ! 5
      val exec = ic.execute(input, output)
      val outputs = mutable.Buffer[Int]()
      while (!exec.isDefined) {
        outputs += Await.result(output.recv.sync())
      }
      outputs.last
    }
  }
}

object Day5Main extends Day5

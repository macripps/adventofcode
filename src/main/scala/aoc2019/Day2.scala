package aoc2019

import aoc.NewDay
import com.twitter.concurrent.Broker
import com.twitter.util.Await

class Day2 extends NewDay(2019, 2) {
  part(1) {
    execute { in =>
      val program = in.head.split(',').map(_.toInt)
      program(1) = 12
      program(2) = 2
      val ic = new IntCode(program)
      val input = new Broker[Int]
      val output = new Broker[Int]
      Await.result(ic.execute(input, output))
      program(0)
    }
  }

  part(2) {
    execute { in =>
      val base = in.head.split(',').map(_.toInt)
      (for {
        noun <- 1 to 100
        verb <- 1 to 100
      } yield {
        val program = base.clone()
        program(1) = noun
        program(2) = verb
        val ic = new IntCode(program)
        val input = new Broker[Int]
        val output = new Broker[Int]
        Await.result(ic.execute(input, output))
        (noun, verb, program(0))
      }).find(_._3 == 19690720).map { case (noun, verb, _) => 100 * noun + verb }.getOrElse(-1)
    }
  }
}

object Day2Main extends Day2

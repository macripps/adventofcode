package aoc2020

import aoc.NewDay

import scala.annotation.tailrec

class Day8 extends NewDay(2020, 8) {
  part(1) {
    execute { in =>
      val result = executeProgram(in)
      "The accumulator value is " + result._2
    }
  }

  part(2) {
    execute { in =>
      val successfulAccumulator = in.indices.flatMap { l =>
        var command: String = in(l)
        if (command.startsWith("nop")) {
          command = "jm" + command.drop(2)
        } else if (command.startsWith("jmp")) {
          command = "no" + command.drop(2)
        }
        Some(executeProgram(in.take(l) ++ Seq(command) ++ in.drop(l + 1)))
      }.filter { r => r._1 }.head._2
      "The accumulator value of the first successful execution is " + successfulAccumulator
    }
  }

  def executeProgram(program: Array[String]): (Boolean, Int) = {
    execute2(program, 0, 0, Set())
  }

  @tailrec
  private[this] def execute2(program: Array[String], instructionCounter: Int, accumulator: Int, executedInstructions: Set[Int]): (Boolean, Int) = {
    if (instructionCounter >= program.length) {
      (true, accumulator)
    } else if (executedInstructions.contains(instructionCounter)) {
      (false, accumulator)
    } else {
      val instruction = program(instructionCounter).split(" ")
      instruction(0) match {
        case "acc" =>
          execute2(program,
            instructionCounter + 1,
            accumulator + instruction(1).toInt,
            executedInstructions + instructionCounter)
        case "jmp" =>
          execute2(program,
            instructionCounter + instruction(1).toInt,
            accumulator,
            executedInstructions + instructionCounter)
        case "nop" =>
          execute2(program,
            instructionCounter + 1,
            accumulator,
            executedInstructions + instructionCounter)
      }
    }
  }
}

object Day8Main extends Day8

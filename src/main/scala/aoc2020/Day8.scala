package aoc2020

import aoc.Day

import scala.annotation.tailrec

class Day8 extends Day {
  override def year: Int = 2020
  override def day: Int = 8

  override def part1(program: Array[String]): String = {
    val result = executeProgram(program)
    "The accumulator value is " + result._2
  }

  override def part2(program: Array[String]): String = {
    val successfulAccumulator = program.indices.flatMap { l =>
      var command: String = program(l)
      if (command.startsWith("nop")) {
        command = "jm" + command.drop(2)
      } else if (command.startsWith("jmp")) {
        command = "no" + command.drop(2)
      }
      Some(executeProgram(program.take(l) ++ Seq(command) ++ program.drop(l + 1)))
    }.filter { r => r._1 }.head._2
    "The accumulator value of the first successful execution is " + successfulAccumulator
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
        case "acc" => {
          execute2(program,
            instructionCounter + 1,
            accumulator + instruction(1).toInt,
            executedInstructions + instructionCounter)
        }
        case "jmp" => {
          execute2(program,
            instructionCounter + instruction(1).toInt,
            accumulator,
            executedInstructions + instructionCounter)
        }
        case "nop" => {
          execute2(program,
            instructionCounter + 1,
            accumulator,
            executedInstructions + instructionCounter)
        }
      }
    }
  }
}

object Day8 {
  def apply() = new Day8()
}

package aoc2019

class IntCode(program: Array[Int]) {

  def execute(): Unit = {
    var ip = 0
    while (program(ip) != 99) {
      program(ip) match {
        case 1 =>
          program(program(ip + 3)) = program(program(ip + 1)) + program(program(ip + 2))
          ip = ip + 4
        case 2 =>
          program(program(ip + 3)) = program(program(ip + 1)) * program(program(ip + 2))
          ip = ip + 4
      }
    }
  }
}

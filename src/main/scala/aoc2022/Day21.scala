package aoc2022

import aoc.NewDay

import aoc2022.Day21._

class Day21 extends NewDay(2022, 21) {
  part(1) {
    execute { in =>
      val root = parse(in, "root")
      root.eval
    }
  }

  part(2) {
    execute { in =>
      val humn = 3441198826073L
      val root1 = parse2(in, "root", humn)
      if (root1.eval == 0) {
        println(humn)
      }
      humn
    }
  }

  def parse(input: Array[String], monkey: String): Shout = {
    input.find(x => x.startsWith(monkey + ": ")).map { l =>
      val rhs = l.drop(monkey.length + 2)
      rhs match {
        case number(n) => Number(n.toInt)
        case add(a, b) => Add(parse(input, a), parse(input, b))
        case minus(a, b) => Minus(parse(input, a), parse(input, b))
        case multiply(a, b) => Multiply(parse(input, a), parse(input, b))
        case divide(a, b) => Divide(parse(input, a), parse(input, b))
        case equal(a, b) => Add(parse(input, a), parse(input, b))
      }
    }.get
  }

  def parse2(input: Array[String], monkey: String, humn: Long): Shout = {
    if (monkey == "humn") {
      Number(humn)
    } else {
      input.find(x => x.startsWith(monkey + ": ")).map { l =>
        val rhs = l.drop(monkey.length + 2)
        rhs match {
          case number(n) => Number(n.toLong)
          case add(a, b) => Add(parse2(input, a, humn), parse2(input, b, humn))
          case minus(a, b) => Minus(parse2(input, a, humn), parse2(input, b, humn))
          case multiply(a, b) => Multiply(parse2(input, a, humn), parse2(input, b, humn))
          case divide(a, b) => Divide(parse2(input, a, humn), parse2(input, b, humn))
          case equal(a, b) => Compare(parse2(input, a, humn), parse2(input, b, humn))
        }
      }.get
    }
  }
}

object Day21Main extends Day21

object Day21 {
  val number = raw"(-?\d+)".r
  val minus = raw"(\w+) - (\w+)".r
  val add = raw"(\w+) \+ (\w+)".r
  val multiply = raw"(\w+) \* (\w+)".r
  val divide = raw"(\w+) / (\w+)".r
  val equal = raw"(\w+) = (\w+)".r

  trait Shout {
    def eval: Long
  }

  case class Number(n: Long) extends Shout {
    override def eval: Long = n
  }

  case class Minus(l: Shout, r: Shout) extends Shout {
    override def eval: Long = l.eval - r.eval
  }

  case class Add(l: Shout, r: Shout) extends Shout {
    override def eval: Long = l.eval + r.eval
  }

  case class Multiply(l: Shout, r: Shout) extends Shout {
    override def eval: Long = l.eval * r.eval
  }

  case class Divide(l: Shout, r: Shout) extends Shout {
    override def eval: Long = l.eval / r.eval
  }

  case class Compare(l: Shout, r: Shout) extends Shout {
    override def eval: Long = {
      val le = l.eval
      val re = r.eval
      le.compareTo(re)
    }
  }
}

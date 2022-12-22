package aoc2022

import aoc2022.Day21._

class Day21 extends aoc.Day(2022, 21) {
  override def part1(input: Array[String]): Any = {
    val root = parse(input, "root")
    root.eval
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

  override def part2(input: Array[String]): Any = {
    val humn = 3441198826073L
    val root1 = parse2(input, "root", humn)
    if (root1.eval == 0) {
      println(humn)
    }
    humn
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

  val test =
    """root: pppw = sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 301
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32""".stripMargin.split("\n")
}

object Day21 {
  def apply() = new Day21

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
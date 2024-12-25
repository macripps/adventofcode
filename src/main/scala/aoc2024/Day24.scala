package aoc2024

import aoc.NewDay

import scala.collection.mutable

class Day24 extends NewDay(2024, 24) {

  part(1) {
    test(
      """x00: 1
        |x01: 1
        |x02: 1
        |y00: 0
        |y01: 1
        |y02: 0
        |
        |x00 AND y00 -> z00
        |x01 XOR y01 -> z01
        |x02 OR y02 -> z02""".stripMargin -> 4)
    test(
      """x00: 1
        |x01: 0
        |x02: 1
        |x03: 1
        |x04: 0
        |y00: 1
        |y01: 1
        |y02: 1
        |y03: 1
        |y04: 1
        |
        |ntg XOR fgs -> mjb
        |y02 OR x01 -> tnw
        |kwq OR kpj -> z05
        |x00 OR x03 -> fst
        |tgd XOR rvg -> z01
        |vdt OR tnw -> bfw
        |bfw AND frj -> z10
        |ffh OR nrd -> bqk
        |y00 AND y03 -> djm
        |y03 OR y00 -> psh
        |bqk OR frj -> z08
        |tnw OR fst -> frj
        |gnj AND tgd -> z11
        |bfw XOR mjb -> z00
        |x03 OR x00 -> vdt
        |gnj AND wpb -> z02
        |x04 AND y00 -> kjc
        |djm OR pbm -> qhw
        |nrd AND vdt -> hwm
        |kjc AND fst -> rvg
        |y04 OR y02 -> fgs
        |y01 AND x02 -> pbm
        |ntg OR kjc -> kwq
        |psh XOR fgs -> tgd
        |qhw XOR tgd -> z09
        |pbm OR djm -> kpj
        |x03 XOR y03 -> ffh
        |x00 XOR y04 -> ntg
        |bfw OR bqk -> z06
        |nrd XOR fgs -> wpb
        |frj XOR qhw -> z04
        |bqk OR frj -> z07
        |y03 OR x01 -> nrd
        |hwm AND bqk -> z03
        |tgd XOR rvg -> z12
        |tnw OR pbm -> gnj""".stripMargin -> 2024)
    execute { ls =>
      val things = mutable.Map[String, Thing]()
      ls.foreach { l =>
        if (l.contains(": ")) { // Wire
          val Array(name, value) = l.split(": ")
          things(name) = Wire(name, value.toInt)
        } else if (l.contains("->")) { // Gate
          val Array(lhs, rhs) = l.split(" -> ")
          val Array(lt, op, rt) = lhs.split(" ")
          op match {
            case "AND" => things(rhs) = And(rhs, lt, rt)
            case "OR" => things(rhs) = Or(rhs, lt, rt)
            case "XOR" => things(rhs) = Xor(rhs, lt, rt)
          }
        }
      }
      var o = 0L
      things.keySet.filter(_.startsWith("z")).toArray.sorted.reverse.foreach { z =>
        o = o << 1L
        o = o | things(z).eval(things)
      }
      o
    }
  }

  part(2) {
    execute { ls =>
      val things = mutable.Map[String, Thing]()
      ls.foreach { l =>
        if (l.contains(": ")) { // Wire
          val Array(name, value) = l.split(": ")
          things(name) = Wire(name, value.toInt)
        } else if (l.contains("->")) { // Gate
          val Array(lhs, rhs) = l.split(" -> ")
          val Array(lt, op, rt) = lhs.split(" ")
          op match {
            case "AND" => things(rhs) = And(rhs, lt, rt)
            case "OR" => things(rhs) = Or(rhs, lt, rt)
            case "XOR" => things(rhs) = Xor(rhs, lt, rt)
          }
        }
      }
      val faultyGates: mutable.ListBuffer[Thing] = mutable.ListBuffer[Thing]()
      /*
       * There are 4 cases in which is faulty:
       *
       * 1. If there is output to a z-wire, the operator should always be XOR (unless
       * it is the final bit). If not -> faulty.
       * 2. If the output is not a z-wire and the inputs are not x and y, the operator
       * should always be AND or OR. If not -> faulty.
       * 3. If the inputs are x and y (but not the first bit) and the operator is XOR,
       * the output wire should be the input of another XOR-gate. If not -> faulty.
       * 4. If the inputs are x and y (but not the first bit) and the operator is AND,
       * the output wire should be the input of an OR-gate. If not -> faulty.
       */
      for (c <- things.values) {
        if (c.name.startsWith("z") && !(c.name.equals("z45"))) {
          if (!(c.isInstanceOf[Xor])) {
            faultyGates.addOne(c)
          }
        }
        else {
          if (!(c.name.startsWith("z")) && !((c.l.startsWith("x") || c.l.startsWith("y"))) && !((c.r.startsWith("x") || c.r.startsWith("y")))) {
            if (c.isInstanceOf[Xor]) {
              faultyGates.addOne(c)
            }
          }
          else {
            if (c.isInstanceOf[Xor] && (c.l.startsWith("x") || c.r.startsWith("y")) && (c.l.startsWith("x") || c.r.startsWith("y"))) {
              if (!((c.l.endsWith("00") && c.r.endsWith("00")))) {
                val output: String = c.name
                var anotherFound: Boolean = false
                for (c2 <- things.values) {
                  if (!(c2.equals(c))) {
                    if ((c2.l.equals(output) || c2.r.equals(output)) && c2.isInstanceOf["Xor"]) {
                      anotherFound = true
                    }
                  }
                }
                if (!(anotherFound)) {
                  faultyGates.addOne(c)
                }
              }
            }
            else {
              if (c.isInstanceOf[And] && (c.l.startsWith("x") || c.l.startsWith("y")) && (c.r.startsWith("x") || c.r.startsWith("y"))) {
                if (!((c.l.endsWith("00") && c.r.endsWith("00")))) {
                  val output: String = c.name
                  var anotherFound: Boolean = false
                  for (c2 <- things.values) {
                    if (!(c2.equals(c))) {
                      if ((c2.l.equals(output) || c2.r.equals(output)) && c2.isInstanceOf[Or]) {
                        anotherFound = true
                      }
                    }
                  }
                  if (!(anotherFound)) {
                    faultyGates.addOne(c)
                  }
                }
              }
            }
          }
        }
      }
      faultyGates.map(_.name).sorted.mkString(",")
    }
  }
}

trait Thing {
  val name: String
  val l: String
  val r: String
  def eval(things: mutable.Map[String, Thing]): Int

  def stringify(things: mutable.Map[String, Thing]): String
}

case class Wire(name: String, value: Int) extends Thing {
  val l = ""
  val r = ""
  override def eval(things: mutable.Map[String, Thing]): Int = value

  override def stringify(things: mutable.Map[String, Thing]): String = name
}

case class And(name: String, l: String, r: String) extends Thing {
  override def eval(things: mutable.Map[String, Thing]): Int = things(l).eval(things) & things(r).eval(things)

  override def stringify(things: mutable.Map[String, Thing]): String = "(" + things(l).stringify(things) + " AND " + things(r).stringify(things) + ")"
}

case class Or(name: String, l: String, r: String) extends Thing {
  override def eval(things: mutable.Map[String, Thing]): Int = things(l).eval(things) | things(r).eval(things)

  override def stringify(things: mutable.Map[String, Thing]): String = "(" + things(l).stringify(things) + " OR " + things(r).stringify(things) + ")"
}

case class Xor(name: String, l: String, r: String) extends Thing {
  override def eval(things: mutable.Map[String, Thing]): Int = things(l).eval(things) ^ things(r).eval(things)

  override def stringify(things: mutable.Map[String, Thing]): String = "(" + things(l).stringify(things) + " XOR " + things(r).stringify(things) + ")"
}

object Day24Main extends Day24

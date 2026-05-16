package aoc2017

import aoc.NewDay

import scala.collection.mutable
import scala.util.matching.Regex

class Day18 extends NewDay(2017, 18) {

  import Day18._

  part(1) {
    execute { in =>
      val p = new Program(0, in, mutable.Map[String, Long]())
      val o = new Program(0, in, mutable.Map[String, Long]())
      p.target = o
      while (p.running && !p.paused) {
        p.step()
      }
      o.inQ.last.toString
    }
  }

  part(2) {
    execute { in =>
      val p0 = new Program(0, in, mutable.Map[String, Long](("p", 0)))
      val p1 = new Program(1, in, mutable.Map[String, Long](("p", 1)))
      p0.target = p1
      p1.target = p0

      while (!(p0.paused && p1.paused)) {
        p0.step()
        p1.step()
      }

      p1.sentValues.toString
    }
  }
}

object Day18 {
  val snd: Regex = raw"snd (\w)".r
  val set: Regex = raw"set (\w) (\w+)".r
  val add: Regex = raw"add (\w) (-?\w+)".r
  val mul: Regex = raw"mul (\w) (-?\w+)".r
  val mod: Regex = raw"mod (\w) (\w+)".r
  val rcv: Regex = raw"rcv (\w)".r
  val jgz: Regex = raw"jgz (\w) (-?\w+)".r

  class Program(id: Int, input: Array[String], val regs: mutable.Map[String, Long]) {
    val inQ: mutable.Queue[Long] = mutable.Queue[Long]()
    var target: Program = _
    var ep = 0
    var sentValues = 0

    def receive(l: Long): Unit = {
      inQ.enqueue(l)
    }

    def running: Boolean = {
      ep >= 0 && ep < input.length
    }

    def paused: Boolean = waitingForInput && inQ.isEmpty

    var waitingForInput = false

    def step(): Unit = {
      input(ep) match {
        case snd(r: String) =>
          target.receive(regs.getOrElse(r, 0))
          sentValues = sentValues + 1
          ep = ep + 1
        case set(r1: String, rOrV: String) =>
          if (rOrV.charAt(0) >= 'a' && rOrV.charAt(0) <= 'z') {
            regs(r1) = regs.getOrElse(rOrV, 0L)
          } else {
            regs(r1) = rOrV.toInt
          }
          ep = ep + 1
        case add(r1: String, rOrV: String) =>
          if (rOrV.charAt(0) >= 'a' && rOrV.charAt(0) <= 'z') {
            regs(r1) = regs.getOrElse(r1, 0L) + regs.getOrElse(rOrV, 0L)
          } else {
            regs(r1) = regs.getOrElse(r1, 0L) + rOrV.toInt
          }
          ep = ep + 1
        case mul(r1: String, rOrV: String) =>
          if (rOrV.charAt(0) >= 'a' && rOrV.charAt(0) <= 'z') {
            regs(r1) = regs.getOrElse(r1, 0L) * regs.getOrElse(rOrV, 0L)
          } else {
            regs(r1) = regs.getOrElse(r1, 0L) * rOrV.toInt
          }
          ep = ep + 1
        case mod(r1: String, rOrV: String) =>
          if (rOrV.charAt(0) >= 'a' && rOrV.charAt(0) <= 'z') {
            regs(r1) = regs.getOrElse(r1, 0L) % regs.getOrElse(rOrV, 0L)
          } else {
            regs(r1) = regs.getOrElse(r1, 0L) % rOrV.toInt
          }
          ep = ep + 1
        case rcv(reg: String) =>
          inQ.headOption match {
            case None => waitingForInput = true
            case Some(i) =>
              waitingForInput = false
              inQ.dequeue()
              regs(reg) = i
              ep = ep + 1
          }
        case jgz(r1: String, rOrV: String) =>
          val v = if (r1.charAt(0) >= 'a' && r1.charAt(0) <= 'z') {
            regs.getOrElse(r1, 0L)
          } else {
            r1.toInt
          }
          if (v > 0) {
            if (rOrV.charAt(0) >= 'a' && rOrV.charAt(0) <= 'z') {
              ep = ep + regs.getOrElse(rOrV, 0L).toInt
            } else {
              ep = ep + rOrV.toInt
            }
          } else {
            ep = ep + 1
          }
      }
    }
  }

}

object Day18Main extends Day18

package aoc2019

import com.twitter.concurrent.{Broker, Offer}
import com.twitter.conversions.DurationOps.richDurationFromInt
import com.twitter.util.{Await, JavaTimer}

class Day7 extends aoc.Day(2019, 7) {
  override def part1(input: Array[String]): Any = {
    val settings = List(0, 1, 2, 3, 4).permutations
    settings.map { oneSetting =>
      val programA = input.head.split(',').map(_.toInt)
      val programB = input.head.split(',').map(_.toInt)
      val programC = input.head.split(',').map(_.toInt)
      val programD = input.head.split(',').map(_.toInt)
      val programE = input.head.split(',').map(_.toInt)
      val in = new Broker[Int]
      val ab = new Broker[Int]
      val bc = new Broker[Int]
      val cd = new Broker[Int]
      val de = new Broker[Int]
      val out = new Broker[Int]
      val ampA = new IntCode(programA).execute(in, ab)
      val ampB = new IntCode(programB).execute(ab, bc)
      val ampC = new IntCode(programC).execute(bc, cd)
      val ampD = new IntCode(programD).execute(cd, de)
      val ampE = new IntCode(programE).execute(de, out)
      val settingsF = for {
        () <- in ! oneSetting.head
        () <- ab ! oneSetting(1)
        () <- bc ! oneSetting(2)
        () <- cd ! oneSetting(3)
        () <- de ! oneSetting(4)
      } ()
      Await.result(settingsF)
      in !! 0
      val o = out.??
      Await.result(ampA)
      Await.result(ampB)
      Await.result(ampC)
      Await.result(ampD)
      Await.result(ampE)
      o
    }.max
  }

  val test = """3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0""".split("\n")

  val timer = new JavaTimer(true)

  override def part2(input: Array[String]): Any = {
    val settings = List(5, 6, 7, 8, 9).permutations
    val prog = input
    settings.map { oneSetting =>
      val programA = prog.head.split(',').map(_.toInt)
      val programB = prog.head.split(',').map(_.toInt)
      val programC = prog.head.split(',').map(_.toInt)
      val programD = prog.head.split(',').map(_.toInt)
      val programE = prog.head.split(',').map(_.toInt)
      val in = new Broker[Int]
      val ab = new Broker[Int]
      val bc = new Broker[Int]
      val cd = new Broker[Int]
      val de = new Broker[Int]
      val out = new Broker[Int]
      val ampA = new IntCode(programA).execute(in, ab)
      val ampB = new IntCode(programB).execute(ab, bc)
      val ampC = new IntCode(programC).execute(bc, cd)
      val ampD = new IntCode(programD).execute(cd, de)
      val ampE = new IntCode(programE).execute(de, out)
      val settingsF = for {
        () <- in ! oneSetting.head
        () <- ab ! oneSetting(1)
        () <- bc ! oneSetting(2)
        () <- cd ! oneSetting(3)
        () <- de ! oneSetting(4)
      } ()
      Await.result(settingsF)
      in !! 0
      var output: Int = 0
      while (!ampA.isDefined) {
        Offer.choose[Option[Int]](
          Offer.timeout(10.seconds)(timer) map { _ => None },
          out.recv.map(n => Some(n))).sync().onSuccess { n =>
          n.foreach { o =>
            output = o
            in ! o
          }
        }
      }
      Await.result(ampA)
      Await.result(ampB)
      Await.result(ampC)
      Await.result(ampD)
      Await.result(ampE)
      output
    }.max
  }

  val test2 = """3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5""".stripMargin.split("\n")
  val test3 = """3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10""".stripMargin.split("\n")
}

object Day7 {
  def apply() = new Day7
}

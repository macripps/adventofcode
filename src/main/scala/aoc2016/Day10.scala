package aoc2016

import aoc.Day

class Day10 extends Day(2016, 10) {
  import aoc2016.Day10._
  override def part1: String = {
    val output = Array.ofDim[Int](21)
    val botsB = Map.newBuilder[Int, Bot]
    input.foreach {
      case i@neitherToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case i@lowToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case i@highToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case i@bothToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case _ =>
    }
    val bots = botsB.result()
    input.foreach {
      case i@valueToBot(value: String, bot: String) => bots(bot.toInt).receive(value.toInt, bots, output)
      case _ =>
    }
    bots.find { case (id, x) => math.min(x.v1.get, x.v2.get) == 17 && math.max(x.v1.get, x.v2.get) == 61 }.head._1.toString
  }

  override def part2: String = {
    val output = Array.ofDim[Int](21)
    val botsB = Map.newBuilder[Int, Bot]
    input.foreach {
      case i@neitherToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case i@lowToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case i@highToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case i@bothToOutput(id: String, _: String, _: String) => botsB.addOne(id.toInt, new Bot(id.toInt, None, None, i))
      case _ =>
    }
    val bots = botsB.result()
    input.foreach {
      case i@valueToBot(value: String, bot: String) => bots(bot.toInt).receive(value.toInt, bots, output)
      case _ =>
    }
    output.take(3).product.toString
  }
}

object Day10 {
  def apply() = new Day10()

  class Bot(val id: Int, var v1: Option[Int], var v2: Option[Int], val inst: String) {
    def receive(i: Int, bots: Map[Int, Bot], output: Array[Int]): Unit = {
      if (!v1.isDefined) {
        v1 = Some(i)
      } else if (!v2.isDefined) {
        v2 = Some(i)
      } else {
        throw new IllegalStateException(this.toString)
      }

      if (v1.isDefined && v2.isDefined) {
        inst match {
          case neitherToOutput(_: String, low: String, high: String) => {
            bots(low.toInt).receive(math.min(v1.get, v2.get), bots, output)
            bots(high.toInt).receive(math.max(v1.get, v2.get), bots, output)
          }
          case lowToOutput(_: String, low: String, high: String) => {
            output(low.toInt) = math.min(v1.get, v2.get)
            bots(high.toInt).receive(math.max(v1.get, v2.get), bots, output)
          }
          case highToOutput(_: String, low: String, high: String) => {
            bots(low.toInt).receive(math.min(v1.get, v2.get), bots, output)
            output(high.toInt) = math.max(v1.get, v2.get)
          }
          case bothToOutput(_: String, low: String, high: String) => {
            output(low.toInt) = math.min(v1.get, v2.get)
            output(high.toInt) = math.max(v1.get, v2.get)
          }
        }
      }
    }
  }


  val valueToBot = raw"value (\d+) goes to bot (\d+)".r
  val neitherToOutput = raw"bot (\d+) gives low to bot (\d+) and high to bot (\d+)".r
  val lowToOutput = raw"bot (\d+) gives low to output (\d+) and high to bot (\d+)".r
  val highToOutput = raw"bot (\d+) gives low to bot (\d+) and high to output (\d+)".r
  val bothToOutput = raw"bot (\d+) gives low to output (\d+) and high to output (\d+)".r

}

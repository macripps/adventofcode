package aoc2019

import aoc.NewDay
import com.twitter.concurrent.{Broker, Offer}
import com.twitter.conversions.DurationOps.richDurationFromInt
import com.twitter.util.{Await, JavaTimer}

class Day7 extends NewDay(2019, 7) {
  val timer = new JavaTimer(true)

  part(1) {
    test("""3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0""" -> 43210)
    execute { in =>
      List(0, 1, 2, 3, 4).permutations.map { oneSetting =>
        val programs = Array.fill(5)(in.head.split(',').map(_.toInt))
        val channels = Array.fill(6)(new Broker[Int])
        val amps = programs.zipWithIndex.map { case (p, i) =>
          new IntCode(p).execute(channels(i), channels(i + 1))
        }
        val settingsF = for {
          () <- channels(0) ! oneSetting.head
          () <- channels(1) ! oneSetting(1)
          () <- channels(2) ! oneSetting(2)
          () <- channels(3) ! oneSetting(3)
          () <- channels(4) ! oneSetting(4)
        } ()
        Await.result(settingsF)
        channels(0) !! 0
        val o = channels(5).??
        amps.foreach(Await.result(_))
        o
      }.max
    }
  }

  part(2) {
    test("""3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5""" -> 139629729)
    execute { in =>
      List(5, 6, 7, 8, 9).permutations.map { oneSetting =>
        val programs = Array.fill(5)(in.head.split(',').map(_.toInt))
        val channels = Array.fill(6)(new Broker[Int])
        val amps = programs.zipWithIndex.map { case (p, i) =>
          new IntCode(p).execute(channels(i), channels(i + 1))
        }
        val settingsF = for {
          () <- channels(0) ! oneSetting.head
          () <- channels(1) ! oneSetting(1)
          () <- channels(2) ! oneSetting(2)
          () <- channels(3) ! oneSetting(3)
          () <- channels(4) ! oneSetting(4)
        } ()
        Await.result(settingsF)
        channels(0) !! 0
        var output = 0
        while (!amps(0).isDefined) {
          Offer.choose[Option[Int]](
            Offer.timeout(10.seconds)(timer).map(_ => None),
            channels(5).recv.map(n => Some(n))
          ).sync().onSuccess { n =>
            n.foreach { o =>
              output = o
              channels(0) ! o
            }
          }
        }
        amps.foreach(Await.result(_))
        output
      }.max
    }
  }
}

object Day7Main extends Day7

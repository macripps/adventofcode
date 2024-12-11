package aoc2024

import com.github.benmanes.caffeine.cache.{AsyncCache, Caffeine}

import java.util.concurrent.CompletableFuture
import scala.annotation.tailrec

class Day11 extends aoc.NewDay(2024, 11) {
  part(1) {
    test("""125 17""" -> 55312)
    execute { ls =>
      val stones = ls.head.split(" ").toSeq.map(_.toLong)
      stones.map(s => stoneDepth(s, 25)).sum
    }
  }

  part(2) {
    execute { ls =>
      val stones = ls.head.split(" ").toSeq.map(_.toLong)
      stones.map(s => stoneDepth(s, 75)).sum
    }
  }

  val cache: AsyncCache[(Long, Int), Long] = Caffeine.newBuilder().recordStats().buildAsync[(Long, Int), Long]()

  private def stoneDepth(v: Long, d: Int): Long = {
    val future = new CompletableFuture[Long]()
    val prior = cache.asMap().putIfAbsent((v, d), future)
    if (prior != null) {
      prior.join()
    } else {
      if (d == 0) {
        future.complete(1)
        1
      } else {
        if (v == 0) {
          val result = stoneDepth(1, d - 1)
          future.complete(result)
          result
        } else if (v.toString.length % 2 == 0) {
          val s = v.toString
          val l = s.take(s.length / 2)
          val r = s.drop(s.length / 2)
          val result = stoneDepth(l.toLong, d - 1) + stoneDepth(r.toLong, d - 1)
          future.complete(result)
          result
        } else {
          val result = stoneDepth(v * 2024, d - 1)
          future.complete(result)
          result
        }
      }
    }
  }

  def size(s: Stone): Long = size(s, 0L)

  @tailrec
  private[this] def size(s: Stone, cur: Long): Long = if (s == null) cur else size(s.next, cur + 1L)


  case class Stone(var value: Long, var next: Stone)
}

object Day11Main extends Day11

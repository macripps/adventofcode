package aoc2024

import aoc.NewDay
import com.github.benmanes.caffeine.cache.{AsyncCache, Caffeine}

import java.util.concurrent.CompletableFuture

class Day19 extends NewDay(2024, 19) {
  part(1) {
    test("""r, wr, b, g, bwu, rb, gb, br
           |
           |brwrr
           |bggr
           |gbbr
           |rrbgbr
           |ubwu
           |bwurrg
           |brgr
           |bbrgwb""".stripMargin -> 6)
    execute { ls =>
      val towels = ls.head.split(", ")
      val patterns = ls.drop(2)
      patterns.count { p =>
        canMake(p, towels)
      }
    }
  }

  part(2) {
    test("""r, wr, b, g, bwu, rb, gb, br
           |
           |brwrr
           |bggr
           |gbbr
           |rrbgbr
           |ubwu
           |bwurrg
           |brgr
           |bbrgwb""".stripMargin -> 16)
    execute { ls =>
      val towels = ls.head.split(", ")
      val patterns = ls.drop(2)

      val cache: AsyncCache[String, Long] = Caffeine.newBuilder().recordStats().buildAsync[String, Long]()

      patterns.map { p =>
        countMakes(cache, p, towels)
      }.sum
    }
  }

  private[this] def canMake(pattern: String, towels: Array[String]): Boolean = {
    val possibleStarts = towels.filter {
      pattern.startsWith
    }
    possibleStarts.exists { p =>
      pattern.length == p.length || canMake(pattern.drop(p.length), towels)
    }
  }

  private[this] def countMakes(cache: AsyncCache[String, Long], pattern: String, towels: Array[String]): Long = {
    val future = new CompletableFuture[Long]()
    val prior = cache.asMap().putIfAbsent(pattern, future)
    if (prior != null) {
      prior.join()
    } else {
      val possibleStarts = towels.filter {
        pattern.startsWith
      }
      val result = possibleStarts.map { p =>
        if (pattern.length == p.length) 1L
        else countMakes(cache, pattern.drop(p.length), towels)
      }.sum
      future.complete(result)
      result
    }
  }
}

object Day19Main extends Day19

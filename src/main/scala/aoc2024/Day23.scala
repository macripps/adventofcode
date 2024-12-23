package aoc2024

import aoc.NewDay
import com.github.benmanes.caffeine.cache.{AsyncCache, Caffeine}

import java.util.concurrent.CompletableFuture
import scala.collection.mutable

class Day23 extends NewDay(2024, 23) {
  part(1) {
    test(
      """kh-tc
        |qp-kh
        |de-cg
        |ka-co
        |yn-aq
        |qp-ub
        |cg-tb
        |vc-aq
        |tb-ka
        |wh-tc
        |yn-cg
        |kh-ub
        |ta-co
        |de-co
        |tc-td
        |tb-wq
        |wh-td
        |ta-ka
        |td-qp
        |aq-cg
        |wq-ub
        |ub-vc
        |de-ta
        |wq-aq
        |wq-vc
        |wh-yn
        |ka-de
        |kh-ta
        |co-tc
        |wh-qp
        |tb-vc
        |td-yn""".stripMargin -> 7L)
    execute { ls =>
      val connections = mutable.Map[String, Set[String]]().withDefault(_ => Set())
      ls.foreach { l =>
        val Array(c1, c2) = l.split('-')
        connections(c1) = connections(c1) + c2
        connections(c2) = connections(c2) + c1
      }
      val triples = mutable.Set[(String, String, String)]()
      connections.keySet.filter(_.startsWith("t")).foreach { c1 =>
        connections(c1).foreach { c2 =>
          connections(c2).foreach { c3 =>
            if (connections(c3).contains(c1)) {
              val s = Array(c1, c2, c3).sorted
              triples += ((s(0), s(1), s(2)))
            }
          }
        }
      }
      triples.size
    }
  }

  part(2) {
    test(
      """kh-tc
        |qp-kh
        |de-cg
        |ka-co
        |yn-aq
        |qp-ub
        |cg-tb
        |vc-aq
        |tb-ka
        |wh-tc
        |yn-cg
        |kh-ub
        |ta-co
        |de-co
        |tc-td
        |tb-wq
        |wh-td
        |ta-ka
        |td-qp
        |aq-cg
        |wq-ub
        |ub-vc
        |de-ta
        |wq-aq
        |wq-vc
        |wh-yn
        |ka-de
        |kh-ta
        |co-tc
        |wh-qp
        |tb-vc
        |td-yn""".stripMargin -> "co,de,ka,ta")
    execute { ls =>
      val connections = mutable.Map[String, Set[String]]().withDefault(_ => Set())
      ls.foreach { l =>
        val Array(c1, c2) = l.split('-')
        connections(c1) = connections(c1) + c2
        connections(c2) = connections(c2) + c1
      }
      cache = Caffeine.newBuilder().buildAsync[Set[String], Set[String]]()
      var biggestGroup = Set[String]()
      connections.keySet.foreach { c1 =>
        val biggest = search(connections, Set(c1))
        if (biggest.size > biggestGroup.size) {
          biggestGroup = biggest
        }
      }
      biggestGroup.toArray.sorted.mkString(",")
    }
  }

  private var cache: AsyncCache[Set[String],Set[String]] = null

  def search(connections: mutable.Map[String, Set[String]], soFar: Set[String]): Set[String] = {
    val future = new CompletableFuture[Set[String]]()
    val prior = cache.asMap().putIfAbsent(soFar, future)
    if (prior != null) {
      prior.join()
    } else {
      var biggestGroup = soFar
      val candidates = soFar.flatMap { c => connections(c) }.diff(soFar)
      candidates.foreach { c =>
        if (soFar.diff(connections(c)).isEmpty) {
          val biggest = search(connections, soFar + c)
          if (biggest.size > biggestGroup.size) {
            biggestGroup = biggest
          }
        }
      }
      future.complete(biggestGroup)
      biggestGroup
    }
  }
}

object Day23Main extends Day23

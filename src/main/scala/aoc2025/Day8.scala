package aoc2025

import aoc.{DisjointSet, Elem, NewDay, Point3}

import scala.collection.mutable

class Day8 extends NewDay(2025, 8) {

  part(1) {
    test(
      """162,817,812
        |57,618,57
        |906,360,560
        |592,479,940
        |352,342,300
        |466,668,158
        |542,29,236
        |431,825,988
        |739,650,466
        |52,470,668
        |216,146,977
        |819,987,18
        |117,168,530
        |805,96,715
        |346,949,466
        |970,615,88
        |941,993,340
        |862,61,35
        |984,92,344
        |425,690,689""".stripMargin -> 40L)
    execute { ls =>
      val pairs = if (ls.length == 20) 10 else 1000
      val distances = mutable.PriorityQueue.empty((x: ((Point3, Point3), Long), y: ((Point3, Point3), Long)) => y._2.compareTo(x._2))
      val pts = ls.map { z =>
        val cd = z.split(",")
        Point3(cd(0).toInt, cd(1).toInt, cd(2).toInt)
      }
      (0 until (pts.length - 1)).foreach { pt1Idx =>
        (pt1Idx + 1 until pts.length).foreach { pt2Idx =>
          val pt1 = pts(pt1Idx)
          val pt2 = pts(pt2Idx)
          distances.addOne((pt1, pt2), pt1.hackyDistanceTo(pt2))
        }
      }
      val djs = new DisjointSet()
      val ptsElemMap = pts.map { pt =>
        (pt, {
          val e = new Elem()
          djs.makeSet(e)
          e
        })
      }.toMap
      (1 to pairs).foreach { pairing =>
        val shortestPair: (Point3, Point3) = distances.dequeue()._1
        djs.union(ptsElemMap(shortestPair._1), ptsElemMap(shortestPair._2))
      }
      val roots = ptsElemMap.values.map(e => djs.find(e)).toSet
      val n = roots.toList.map { e => (e, e.size) }
      n.sortBy(-_._2).take(3).map(_._2).product
    }
  }

  part(2) {
    test(
      """162,817,812
        |57,618,57
        |906,360,560
        |592,479,940
        |352,342,300
        |466,668,158
        |542,29,236
        |431,825,988
        |739,650,466
        |52,470,668
        |216,146,977
        |819,987,18
        |117,168,530
        |805,96,715
        |346,949,466
        |970,615,88
        |941,993,340
        |862,61,35
        |984,92,344
        |425,690,689""".stripMargin -> 25272)
    execute { ls =>
      val distances = mutable.PriorityQueue.empty((x: ((Point3, Point3), Long), y: ((Point3, Point3), Long)) => y._2.compareTo(x._2))
      val pts = ls.map { z =>
        val cd = z.split(",")
        Point3(cd(0).toInt, cd(1).toInt, cd(2).toInt)
      }
      (0 until (pts.length - 1)).foreach { pt1Idx =>
        (pt1Idx + 1 until pts.length).foreach { pt2Idx =>
          val pt1 = pts(pt1Idx)
          val pt2 = pts(pt2Idx)
          distances.addOne((pt1, pt2), pt1.hackyDistanceTo(pt2))
        }
      }
      val djs = new DisjointSet()
      val ptsElemMap = pts.map { pt =>
        (pt, {
          val e = new Elem()
          djs.makeSet(e)
          e
        })
      }.toMap
      var shortestPair: (Point3, Point3) = null
      while (ptsElemMap.values.map(e => djs.find(e)).toSet.size > 1) {
        val roots = ptsElemMap.values.map(e => djs.find(e)).toSet
        val n = roots.toList.map { e => (e, e.size) }
        shortestPair = distances.dequeue()._1
        djs.union(ptsElemMap(shortestPair._1), ptsElemMap(shortestPair._2))
      }
      shortestPair._1.x.toLong * shortestPair._2.x.toLong
    }
  }
}

object Day8Main extends Day8
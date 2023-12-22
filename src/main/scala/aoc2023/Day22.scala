package aoc2023

import aoc.{NewDay, Point3}

import scala.collection.mutable

class Day22 extends NewDay(2023, 22) {

  part(1) {
    test {
      """1,0,1~1,2,1
        |0,0,2~2,0,2
        |0,2,3~2,2,3
        |0,0,4~0,2,4
        |2,0,5~2,2,5
        |0,1,6~2,1,6
        |1,1,8~1,1,9""".stripMargin -> 5
    }

    execute { input =>
      val blocks = input.map { line =>
        val Array(start, end) = line.split('~')
        val Array(startX, startY, startZ) = start.split(',')
        val Array(endX, endY, endZ) = end.split(',')
        (math.min(startX.toInt, endX.toInt) to math.max(startX.toInt, endX.toInt)).flatMap { x =>
          (math.min(startY.toInt, endY.toInt) to math.max(startY.toInt, endY.toInt)).flatMap { y =>
            (math.min(startZ.toInt, endZ.toInt) to math.max(startZ.toInt, endZ.toInt)).map { z => Point3(x, y, z) }
          }
        }
      }
      var blockFell = true
      while (blockFell) {
        blockFell = false
        blocks.zipWithIndex.foreach { case (b, i) =>
          val minZ = b.map(_.z).min
          if (minZ > 1) {
            val lowestPoints = b.filter { p => p.z == minZ }
            val unsupported = lowestPoints.forall { pt =>
              !blocks.exists(b => b.exists { pt2 => pt2.x == pt.x && pt2.y == pt.y && pt2.z == pt.z - 1 })
            }
            if (unsupported) {
              blockFell = true
              blocks(i) = b.map { pt => pt.copy(z = pt.z - 1) }
            }
          }
        }
      }
      val supportersRMap = mutable.Map[Int, Set[Int]]().withDefaultValue(Set())
      blocks.zipWithIndex.foreach { case (b, i) =>
        if (!supportersRMap.contains(i)) supportersRMap(i) = Set()
        val minZ = b.map(_.z).min
        val lowestPoints = b.filter { p => p.z == minZ }
        lowestPoints.foreach { pt =>
          blocks.zipWithIndex.foreach {
            case (b, i2) =>
              if (b.exists { pt2 => pt2.x == pt.x && pt2.y == pt.y && pt2.z == pt.z - 1 }) {
                supportersRMap(i) += i2
              }
          }
        }
      }
      blocks.indices.count { i =>
        supportersRMap.values.forall(s => !s.contains(i) || s.size >= 2)
      }
    }
  }

  part(2) {
    test {
      """1,0,1~1,2,1
        |0,0,2~2,0,2
        |0,2,3~2,2,3
        |0,0,4~0,2,4
        |2,0,5~2,2,5
        |0,1,6~2,1,6
        |1,1,8~1,1,9""".stripMargin -> 7
    }

    execute { input =>
      val blocks = input.map { line =>
        val Array(start, end) = line.split('~')
        val Array(startX, startY, startZ) = start.split(',')
        val Array(endX, endY, endZ) = end.split(',')
        (math.min(startX.toInt, endX.toInt) to math.max(startX.toInt, endX.toInt)).flatMap { x =>
          (math.min(startY.toInt, endY.toInt) to math.max(startY.toInt, endY.toInt)).flatMap { y =>
            (math.min(startZ.toInt, endZ.toInt) to math.max(startZ.toInt, endZ.toInt)).map { z => Point3(x, y, z) }
          }
        }
      }
      var blockFell = true
      while (blockFell) {
        blockFell = false
        blocks.zipWithIndex.foreach { case (b, i) =>
          val minZ = b.map(_.z).min
          if (minZ > 1) {
            val lowestPoints = b.filter { p => p.z == minZ }
            val unsupported = lowestPoints.forall { pt =>
              !blocks.exists(b => b.exists { pt2 => pt2.x == pt.x && pt2.y == pt.y && pt2.z == pt.z - 1 })
            }
            if (unsupported) {
              blockFell = true
              blocks(i) = b.map { pt => pt.copy(z = pt.z - 1) }
            }
          }
        }
      }
      val supportersRMap = mutable.Map[Int, Set[Int]]().withDefaultValue(Set())
      blocks.zipWithIndex.foreach { case (b, i) =>
        if (!supportersRMap.contains(i)) supportersRMap(i) = Set()
        val minZ = b.map(_.z).min
        val lowestPoints = b.filter { p => p.z == minZ }
        lowestPoints.foreach { pt =>
          blocks.zipWithIndex.foreach {
            case (b, i2) =>
              if (b.exists { pt2 => pt2.x == pt.x && pt2.y == pt.y && pt2.z == pt.z - 1 }) {
                supportersRMap(i) += i2
              }
          }
        }
      }
      blocks.indices.map { i =>
        countFallers(i, supportersRMap)
      }.sum
    }
  }

  def countFallers(i: Int, supportersRMap: mutable.Map[Int, Set[Int]]): Int = {
    val falling = mutable.Set[Int](i)
    var moreFall = true
    while (moreFall) {
      val fallers = supportersRMap.filter { case (_, v) => v.nonEmpty && v.diff(falling).isEmpty }
      moreFall = fallers.keySet.diff(falling).nonEmpty
      falling.addAll(fallers.keys)
    }
    falling.size - 1
  }


}

object Day22Main extends Day22

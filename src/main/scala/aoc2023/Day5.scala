package aoc2023

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day5 extends aoc.Day(2023, 5) {

  import Day5._

  withPart1Test(
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin, 35L)

  override def part1(input: Array[String]): Any = {
    val groups = inputGroups(input)
    val seeds = groups.head.head.drop("seeds: ".length).split(' ').map(_.toLong)
    seeds.map { seed =>
      var n = seed
      val maps = groups.tail
      maps.foreach { mapping =>
        val ranges = mapping.tail
        breakable {
          ranges.foreach { range =>
            val Array(dest, src, length) = range.split(' ').map(_.toLong)
            if (n >= src && n <= src + length) {
              n = dest + (n - src)
              break()
            }
          }
        }
      }
      n
    }.min
  }

  withPart2Test(
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin, 46L)

  override def part2(input: Array[String]): Any = {
    val seedRange = input.head.split(": ")(1).split(" ")
    var seeds = new SeedRangeList()
    seedRange.grouped(2).foreach { seedRanges =>
      seeds.add(seedRanges(0).toLong, seedRanges(0).toLong + seedRanges(1).toLong)
    }
    val seedMaps = mutable.Buffer[SeedMap]()
    var currentMap: SeedMap = null
    input.tail.foreach { line =>
      if (line != "") {
        if (line.endsWith("map:")) {
          val Array(off, to) = line.split(' ').head.split("-to-", 2)
          currentMap = new SeedMap(off, to)
          seedMaps.append(currentMap)
        } else {
          val l = line.split(' ')
          currentMap.add(l(0).toLong, l(1).toLong, l(2).toLong)
        }
      }
    }
    seedMaps.foreach { seedMap =>
      seedMap.sort()
    }
    seedMaps.foreach { seedMap =>
      seeds = seedMap.getDestination(seeds)
    }

    seeds.ranges.map { case (start, _) => start }.min
  }
}

object Day5 {
  def apply() = new Day5

  // Thanks to https://github.com/scorixear/AdventOfCode/blob/main/2023/5/2.py for the implementation
  class SeedMap(off: String, to: String) {
    var mapValues = mutable.Buffer[(Long, Long, Long)]()

    def add(target: Long, source: Long, offset: Long): Unit = {
      mapValues.append((source, source + offset, target))
    }

    def sort(): Unit = {
      mapValues = mapValues.sortBy(_._1)
    }

    def getDestination(ranges: SeedRangeList): SeedRangeList = {
      val newRanges = new SeedRangeList()

      ranges.ranges.foreach { case (s, end) =>
        var start = s
        var found_mapping = false
        breakable {
          mapValues.foreach { case (source, max_source, target) =>
            if (start < source) {
              if (end < source) {
                newRanges.add(start, end)
                found_mapping = true
                break()
              }
              if (end <= max_source) {
                newRanges.add(start, source)
                newRanges.add(target, target + (end - source))
                found_mapping = true
                break()
              }
              newRanges.add(start, source)
              newRanges.add(target, target + (max_source - source))
              start = max_source
            } else if (source <= start && start < max_source) {
              if (end <= max_source) {
                newRanges.add(target + (start - source), target + (end - source))
                found_mapping = true
                break()
              }
              newRanges.add(target + (start - source), target + (max_source - source))
              start = max_source
            }
          }
        }
        if (!found_mapping) {
          newRanges.add(start, end)
        }
      }
      newRanges
    }

    override def toString: String = s"${off} => ${to}"
  }

  class SeedRangeList() {
    var ranges = mutable.Buffer[(Long, Long)]()

    def add(st: Long, end: Long): Unit = {
      var start = st

      ranges.zipWithIndex.toList.foreach { case ((s, e), i) =>
        if (start < s) {
          if (end < s) {
            ranges.insert(i, (start, end))
            return
          }
          if (end <= e) {
            ranges.insert(i, (start, s))
            return
          }
          ranges.insert(i, (start, s))
          start = e
        } else if (start <= s && s <= end) {
          if (end <= e) {
            return
          }
          start = e
        }
      }
      ranges.append((start, end))
    }
  }
}

package aoc2022

import aoc2022.Day19._

class Day19 extends aoc.Day(2022, 19) {
  override def part1(input: Array[String]): Any = {
    val blueprints = test.map {
      case line(id, cost1, cost2, cost3, cost4, cost5, cost6) =>
        Blueprint(id.toInt, cost1.toInt, cost2.toInt, cost3.toInt, cost4.toInt, cost5.toInt, cost6.toInt)
    }
    blueprints.map { bp =>
      println(s"=== Blueprint ${bp.id} ===")
      bp.id * part1search(bp, 24)
    }.sum
  }

  def part1search(bp: Blueprint, time: Int): Int = {
    val maxRobots = (
      math.max(math.max(bp.oreRobotOreCost, bp.clayRobotOreCost), math.max(bp.obsidianRobotOreCost, bp.geodeRobotOreCost)),
      bp.obsidianRobotClayCost
    )
    var maxGeode = 0;

    def search(time: Int, oreRobots: Int, clayRobots: Int, obsidianRobots: Int, ore: Int, clay: Int, obsidian: Int, geodes: Int): Unit = {
      if (time < 1) () else {
        if (geodes > maxGeode) {
          maxGeode = geodes;
        }

        //Build geode robot
        val canBuildGeodeNow =
          bp.geodeRobotOreCost <= ore && bp.geodeRobotObsidianCost <= obsidian;
        if (obsidianRobots > 0) {
          val timeSkip = 1 + (if (canBuildGeodeNow) 0 else math.max(
            Math.ceil((bp.geodeRobotOreCost - ore) / oreRobots).toInt,
            Math.ceil((bp.geodeRobotObsidianCost - obsidian) / obsidianRobots).toInt
          ))

          search(
            time - timeSkip,
            oreRobots,
            clayRobots,
            obsidianRobots,
            ore + (timeSkip * oreRobots) - bp.geodeRobotOreCost,
            clay + (timeSkip * clayRobots),
            obsidian + (timeSkip * obsidianRobots) - bp.geodeRobotObsidianCost,
            geodes + time - timeSkip
          )

        }
        if (!canBuildGeodeNow) {
          //Build obsidian robot
          if (clayRobots > 0) {
            val canBuildObsidianNow = bp.obsidianRobotOreCost <= ore && bp.obsidianRobotClayCost <= clay;
            val timeSkip = 1 +
              (if (canBuildObsidianNow) 0 else math.max(
                Math.ceil((bp.obsidianRobotOreCost - ore) / oreRobots).toInt,
                Math.ceil((bp.obsidianRobotClayCost - clay) / clayRobots).toInt
              ))

            if (time - timeSkip > 2) {
              search(
                time - timeSkip,
                oreRobots,
                clayRobots,
                obsidianRobots + 1,
                ore + timeSkip * oreRobots - bp.obsidianRobotOreCost,
                clay + timeSkip * clayRobots - bp.obsidianRobotClayCost,
                obsidian + timeSkip * obsidianRobots,
                geodes
              )
            }
          }

          //Build clay robot
          if (clayRobots < maxRobots._2) {
            val canBuildClayNow = bp.clayRobotOreCost <= ore;
            val timeSkip =
              1 + (if (canBuildClayNow) 0 else math.ceil((bp.clayRobotOreCost - ore) / oreRobots).toInt)

            if (time - timeSkip > 3) {
              search(
                time - timeSkip,
                oreRobots,
                clayRobots + 1,
                obsidianRobots,
                ore + timeSkip * oreRobots - bp.clayRobotOreCost,
                clay + timeSkip * clayRobots,
                obsidian + timeSkip * obsidianRobots,
                geodes
              )
            }
          }

          //Build ore robot
          if (oreRobots < maxRobots._1) {
            val canBuildOreNow = bp.oreRobotOreCost <= ore;
            val timeSkip =
              1 + (if (canBuildOreNow) 0 else Math.ceil((bp.oreRobotOreCost - ore) / oreRobots).toInt)

            if (time - timeSkip > 4) {
              search(
                time - timeSkip,
                oreRobots + 1,
                clayRobots,
                obsidianRobots,
                ore + timeSkip * oreRobots - bp.oreRobotOreCost,
                clay + timeSkip * clayRobots,
                obsidian + timeSkip * obsidianRobots,
                geodes
              )
            }
          }
        }
      }
      ()
    }

    search(time, 1, 0, 0, 0, 0, 0, 0)
    println("Blueprint max: " + maxGeode)
    maxGeode
  }

  val test =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = "???"
}

object Day19 {
  def apply() = new Day19

  val line = raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.".r

  case class Blueprint(id: Int, oreRobotOreCost: Int, clayRobotOreCost: Int, obsidianRobotOreCost: Int, obsidianRobotClayCost: Int, geodeRobotOreCost: Int, geodeRobotObsidianCost: Int)

}
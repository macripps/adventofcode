package aoc2020

import aoc.Day
import aoc2020.Day4.{toPassport, validPart1, validPart2}

class Day4 extends Day(2020, 4) {
  override def part1: String = {
    "There were " + inputGroups.map(toPassport).count(validPart1) + " valid passports"
  }

  override def part2: String = {
    "There were " + inputGroups.map(toPassport).count(validPart2) + " valid passports"
  }
}

object Day4 {
  def apply() = new Day4()

  def toPassport(lines: Iterable[String]): Map[String, String] = {
    lines.flatMap { l =>
      l.split(" ").map { p =>
        val kv = p.split(":")
        kv(0) -> kv(1)
      }
    }.toMap
  }

  def validPart1(value: Map[String, String]): Boolean = {
    value.contains("byr") &&
      value.contains("iyr") &&
      value.contains("eyr") &&
      value.contains("hgt") &&
      value.contains("hcl") &&
      value.contains("ecl") &&
      value.contains("pid")
  }

  def validPart2(value: Map[String, String]): Boolean = {
    val valid = value.contains("byr") && value("byr").length == 4 && value("byr").toInt >= 1920 && value("byr").toInt <= 2002 &&
      value.contains("iyr") && value("iyr").length == 4 && value("iyr").toInt >= 2010 && value("iyr").toInt <= 2020 &&
      value.contains("eyr") && value("eyr").length == 4 && value("eyr").toInt >= 2020 && value("eyr").toInt <= 2030 &&
      value.contains("hgt") && ((value("hgt").endsWith("cm") && value("hgt").stripSuffix("cm").toInt >= 150 && value("hgt").stripSuffix("cm").toInt <= 193) || (value("hgt").endsWith("in") && value("hgt").stripSuffix("in").toInt >= 59 && value("hgt").stripSuffix("in").toInt <= 76)) &&
      value.contains("hcl") && value("hcl").matches("^#[0-9a-f]{6}$") &&
      value.contains("ecl") && Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value("ecl")) &&
      value.contains("pid") && value("pid").length == 9
    valid
  }

}

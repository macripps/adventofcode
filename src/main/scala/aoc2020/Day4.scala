package aoc2020

import scala.collection.mutable

object Day4 {

  def main(): Unit = {
    val passports = toPassports(readFileToIterable("aoc2020/day4.input"))
    val validPassports = passports.count(valid)

    println("There were " + validPassports + " valid passports")
  }

  def toPassports(lines: Iterable[String]): Iterable[Map[String, String]] = {
    var passports = Seq[Map[String, String]]()
    var currentPassport = Map[String, String]()
    lines.foreach { l =>
      if (l.equals("")) {
        passports = passports :+ currentPassport
        currentPassport = Map[String, String]()
      } else {
        val pairs = l.split(" ")
        println(pairs.mkString(","))
        pairs.foreach { p =>
          val kv = p.split(":")
          currentPassport += (kv(0) -> kv(1))
        }
      }
    }
    if (currentPassport.nonEmpty) {
      passports = passports :+ currentPassport
    }
    passports
  }

  def valid(value: Map[String, String]): Boolean = {
    println(value)
    val valid = value.contains("byr") && value("byr").length == 4 && value("byr").toInt >= 1920 && value("byr").toInt <= 2002 &&
      value.contains("iyr") && value("iyr").length == 4 && value("iyr").toInt >= 2010 && value("iyr").toInt <= 2020 &&
      value.contains("eyr") && value("eyr").length == 4 && value("eyr").toInt >= 2020 && value("eyr").toInt <= 2030 &&
      value.contains("hgt") && ((value("hgt").endsWith("cm") && value("hgt").stripSuffix("cm").toInt >= 150 && value("hgt").stripSuffix("cm").toInt <= 193) || (value("hgt").endsWith("in") && value("hgt").stripSuffix("in").toInt >= 59 && value("hgt").stripSuffix("in").toInt <= 76)) &&
      value.contains("hcl") && value("hcl").matches("^#[0-9a-f]{6}$") &&
      value.contains("ecl") && Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value("ecl")) &&
      value.contains("pid") && value("pid").length == 9
    println(valid)
    valid
  }

}

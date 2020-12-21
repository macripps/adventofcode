package aoc2015

import aoc.Day
import Day12._

import spray.json._
import DefaultJsonProtocol._

class Day12 extends Day(2015, 12) {
  override def part1: String = {
    val line = input(0)
    var sum = 0
    var i = 0
    while (i < line.length) {
      if (line(i) == '-' || (line(i) >= '0' && line(i) <= '9')) {
        var j = i + 1
        while (line(j) >= '0' && line(j) <= '9') {
          j = j + 1
        }
        sum = sum + line.substring(i, j).toInt
        i = j
      } else {
        i = i + 1
      }
    }
    sum.toString
  }

  override def part2: String = {
    val source = input(0).parseJson
    val obj = source.asJsObject
    sum(obj).toString
  }
}

object Day12 {
  def apply() = new Day12()

  def sum(jsValue: JsValue): Int = {
    jsValue match {
      case JsArray(elems) => elems.map(sum).sum
      case JsObject(fields) => {
        if (fields.exists { fv => fv._2.isInstanceOf[JsString] && fv._2.convertTo[String] == "red"}) 0 else {
          fields.map { fv => sum(fv._2) }.sum
        }
      }
      case JsNumber(x) => x.toInt
      case _ => 0
    }
  }
}

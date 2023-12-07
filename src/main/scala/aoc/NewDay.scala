package aoc

import com.twitter.inject.app.App

abstract class NewDay(year: Int, day: Int) extends App with AdventDSL {
  val debug = flag("debug", false, "enable debug mode")

  override def run(): Unit = {

  }
}

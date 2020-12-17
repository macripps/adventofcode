package aoc

import aoc2015._
import com.twitter.app.App
import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.context.Context
import io.opentelemetry.exporter.zipkin.ZipkinSpanExporter
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor

object AdventOfCode extends App {

  def main(): Unit = {
    val tracerManagement = OpenTelemetrySdk.getGlobalTracerManagement
    val exporter = ZipkinSpanExporter.builder().setServiceName("aoc").build()
    val simpleSpanProcessor = SimpleSpanProcessor.builder(exporter).build()
    tracerManagement.addSpanProcessor(simpleSpanProcessor)

    println("Advent Of Code")
    println("--------------")


    //    val days = List(Day1(), Day2(), Day3(), Day4(), Day5(), Day6(), Day7(), Day8(), Day9(), Day10(), Day11(), Day12(), Day13(), Day14(), Day15(), Day16(), Day17())
    val days = List(Day6())

    val tracer = OpenTelemetry.getGlobalTracer("aoc")
    val compSpan = tracer.spanBuilder("adventofcode").startSpan()
    val compScope = compSpan.makeCurrent()
    days.foreach { day =>
      val daySpan = tracer.spanBuilder("" + day.year + ": day" + day.day).setParent(Context.current().`with`(compSpan)).startSpan()

      val part1Span = tracer.spanBuilder("part1").setParent(Context.current().`with`(daySpan)).startSpan()
      val part1Scope = part1Span.makeCurrent()
      val result1 = day.part1
      part1Scope.close()
      part1Span.`end`()
      printf("%d.%d.1: %s\n", day.year, day.day, result1)

      val part2Span = tracer.spanBuilder("part2").setParent(Context.current().`with`(daySpan)).startSpan()
      val part2Scope = part2Span.makeCurrent()
      val result2 = day.part2
      part2Scope.close()
      part2Span.`end`()
      printf("%d.%d.2: %s\n", day.year, day.day, result2)

      daySpan.`end`()
    }
    compScope.close()
    compSpan.`end`()

    simpleSpanProcessor.shutdown()
  }
}

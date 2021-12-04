package aoc

import aoc2021._
import com.twitter.app.{App, Flag}
import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.context.Context
import io.opentelemetry.exporter.zipkin.ZipkinSpanExporter
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.resources.Resource
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.BatchSpanProcessor
import io.opentelemetry.semconv.resource.attributes.ResourceAttributes

import java.util.concurrent.TimeUnit

import scala.collection.parallel.CollectionConverters._

object AdventOfCode extends App {

  override def failfastOnFlagsNotParsed: Boolean = true

  val debug: Flag[Boolean] = flag("debug", false, "enable debug mode")

  def main(): Unit = {
    val serviceNameResource: Resource = Resource.create(Attributes.of(ResourceAttributes.SERVICE_NAME, "adventofcode"))
    val tracerProvider: SdkTracerProvider = SdkTracerProvider.builder()
      .addSpanProcessor(BatchSpanProcessor.builder(ZipkinSpanExporter.builder().build()).build())
      .setResource(Resource.getDefault.merge(serviceNameResource))
      .build()

    val telemetry: OpenTelemetry = OpenTelemetrySdk.builder()
      .setTracerProvider(tracerProvider)
      .buildAndRegisterGlobal()

    println("Advent Of Code")
    println("--------------")

    //    val days = Array(Day1(), Day2(), Day3(), Day4(), Day5(), Day6(), Day7(), Day8(), Day9(), Day10(), Day11(), Day12(), Day13(), Day14(), Day15(), Day16(), Day17(), Day18(), Day19(), Day20(), Day21(), Day22())
    val days = Array(Day4())

    val tracer = telemetry.getTracer("aoc")
    val compSpan = tracer.spanBuilder("adventofcode").startSpan()
    val compScope = compSpan.makeCurrent()
    days.foreach { day =>
      if (debug()) {
        day.debug = true
      }
      val daySpan = tracer.spanBuilder("" + day.year + ": day" + day.day).setParent(Context.current().`with`(compSpan)).startSpan()

      val input: Array[String] = {
        val inputSpan = tracer.spanBuilder("read_input").setParent(Context.current().`with`(daySpan)).startSpan()
        val input = readFileToIterable("aoc" + day.year + "/day" + day.day + ".input").toArray
        inputSpan.`end`()
        input
      }

      val result1 = {
        val part1Span = tracer.spanBuilder("part1").setParent(Context.current().`with`(daySpan)).startSpan()
        val part1Scope = part1Span.makeCurrent()
        val out = day.part1(input)
        part1Scope.close()
        part1Span.`end`()
        out
      }
      printf("%d.%d.1: %s\n", day.year, day.day, result1)

      val result2 = {
        val part2Span = tracer.spanBuilder("part2").setParent(Context.current().`with`(daySpan)).startSpan()
        val part2Scope = part2Span.makeCurrent()
        val out = day.part2(input)
        part2Scope.close()
        part2Span.`end`()
        out
      }
      printf("%d.%d.2: %s\n", day.year, day.day, result2)

      daySpan.`end`()
    }
    compScope.close()
    compSpan.`end`()

    tracerProvider.forceFlush().join(10, TimeUnit.SECONDS)
    tracerProvider.shutdown()
  }
}

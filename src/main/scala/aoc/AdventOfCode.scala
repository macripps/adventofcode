package aoc

import aoc2023._
import com.twitter.app.Flag
import com.twitter.inject.app.App
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

object AdventOfCode extends App {

  override def failfastOnFlagsNotParsed: Boolean = true

  val debug: Flag[Boolean] = flag("debug", false, "enable debug mode")

  override def run(): Unit = {
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

    val days: Array[aoc.Day] = Array(Day6())

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

      val part1TestsPass = {
        val part1TestSpan = tracer.spanBuilder("part1Tests").setParent(Context.current().`with`(daySpan)).startSpan()
        val part1TestScope = part1TestSpan.makeCurrent()
        val out = day.runPart1Tests
        part1TestScope.close()
        part1TestSpan.`end`()
        out
      }

      if (part1TestsPass) {
        val result1 = {
          val part1Span = tracer.spanBuilder("part1").setParent(Context.current().`with`(daySpan)).startSpan()
          val part1Scope = part1Span.makeCurrent()
          val out = day.part1(input)
          part1Scope.close()
          part1Span.`end`()
          out
        }
        printf("%d.%d.1: %s\n", day.year, day.day, result1.toString)


        val part2TestsPass = {
          val part2TestSpan = tracer.spanBuilder("part2Tests").setParent(Context.current().`with`(daySpan)).startSpan()
          val part2TestScope = part2TestSpan.makeCurrent()
          val out = day.runPart2Tests
          part2TestScope.close()
          part2TestSpan.`end`()
          out
        }

        if (part2TestsPass) {
          val result2 = {
            val part2Span = tracer.spanBuilder("part2").setParent(Context.current().`with`(daySpan)).startSpan()
            val part2Scope = part2Span.makeCurrent()
            val out = day.part2(input)
            part2Scope.close()
            part2Span.`end`()
            out
          }
          printf("%d.%d.2: %s\n", day.year, day.day, result2.toString)
        }
      }

      daySpan.`end`()
    }
    compScope.close()
    compSpan.`end`()

    tracerProvider.forceFlush().join(10, TimeUnit.SECONDS)
    tracerProvider.shutdown()
  }
}

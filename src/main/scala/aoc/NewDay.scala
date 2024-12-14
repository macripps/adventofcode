package aoc

import com.twitter.inject.app.App
import io.opentelemetry.api.GlobalOpenTelemetry
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.exporter.zipkin.ZipkinSpanExporter
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.resources.Resource
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.BatchSpanProcessor
import io.opentelemetry.semconv.resource.attributes.ResourceAttributes

import java.util.concurrent.TimeUnit

abstract class NewDay(year: Int, day: Int) extends App with AdventDSL {
  val debug = flag("debug", false, "enable debug mode")

  init {
    println(System.getProperty("user.dir"))
    val serviceNameResource: Resource = Resource.create(Attributes.of(ResourceAttributes.SERVICE_NAME, "adventofcode"))
    val tracerProvider: SdkTracerProvider = SdkTracerProvider.builder()
      .addSpanProcessor(BatchSpanProcessor.builder(ZipkinSpanExporter.builder().build()).build())
      .setResource(Resource.getDefault.merge(serviceNameResource))
      .build()

    onExit {
      tracerProvider.forceFlush().join(10, TimeUnit.SECONDS)
      tracerProvider.shutdown()
    }

    OpenTelemetrySdk.builder()
      .setTracerProvider(tracerProvider)
      .buildAndRegisterGlobal()
  }

  override def run(): Unit = {
    trace(s"${year}.${day}") {
      val input = trace("parse_input") {
        readFileToIterable("aoc" + year + "/day" + day + ".input").toArray
      }

      trace("part_1") {
        if (executes.contains(1)) {
          val part1TestsPass = trace("tests") {
            runTests(1)
          }

          if (part1TestsPass) {
            val result1 = trace("execute") {
              executes(1)(input)
            }
            info(String.format("%d.%d.1: %s", year, day, result1.toString))
          }
        }
      }

      trace("part_2") {
        if (executes.contains(2)) {
          val part2TestsPass = trace("tests") {
            runTests(2)
          }

          if (part2TestsPass) {
            val result2 = trace("execute") {
              executes(2)(input)
            }
            info(String.format("%d.%d.2: %s", year, day, result2.toString))
          }
        }
      }
    }
  }

  private[this] def runTests(part: Int): Boolean = {
    !testCases.contains(part) || {
      debug.let(true) {
        testCases(part).zipWithIndex.forall {
          { e =>
            val span = GlobalOpenTelemetry.get().getTracer("aoc").spanBuilder(s"testcase_${e._2}").startSpan()
            val result = executes(part)(e._1._1)
            val testPass = result == e._1._2
            info(String.format("%d.%d.%d.T%d: %s/%s: %s", year, day, part, e._2, result, e._1._2, if (testPass) {
              "✅"
            } else {
              "❌"
            }))
            span.end()
            testPass
          }
        }
      }
    }
  }

  private[this] def trace[T](name: String)(f: => T): T = {
    val tracer = GlobalOpenTelemetry.get().getTracer("aoc")
    val span = tracer.spanBuilder(name).startSpan()
    val scope = span.makeCurrent()
    val r: T = f
    scope.close()
    span.end()
    r
  }
}

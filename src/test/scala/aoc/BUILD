load("@rules_scala//scala:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "advent_dsl_suite",
    srcs = ["AdventDSLSuite.scala"],
    visibility = ["//visibility:public"],
    deps = [
        "//src/main/scala/aoc",
        "@rules_scala//testing/toolchain:scalatest_classpath",
    ],
)

scala_test(
    name = "AocTest",
    srcs = ["Day13Test.scala"],
    deps = [
        "//src/main/scala/aoc",
    ],
)

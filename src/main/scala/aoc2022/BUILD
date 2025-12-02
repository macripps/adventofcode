load("//tools/build_rules:aoc.bzl", "advent_of_code_binary")
load("@rules_scala//scala:scala.bzl", "scala_library")

scala_library(
    name = "aoc2022",
    srcs = glob(["*.scala"]),
    deps = [
        "//src/main/scala/aoc"
    ],
    visibility = [
        "//visibility:public",
    ],
    resources = [
        "//src/main/resources/aoc2022:day16.input",
    ]
)

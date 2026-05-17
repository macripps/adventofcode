load("@rules_scala//scala:scala.bzl", "scala_library")
load("//tools/build_rules:aoc.bzl", "advent_of_code_binary")

scala_library(
    name = "aoc2022",
    srcs = glob(["*.scala"]),
    visibility = [
        "//visibility:public",
    ],
    deps = [
        "//src/main/scala/aoc",
        "@maven//:com_github_ben_manes_caffeine_caffeine",
        "@maven//:com_google_inject_guice",
        "@maven//:com_twitter_inject_app_2_13",
        "@maven//:com_twitter_inject_core_2_13",
        "@maven//:com_twitter_util_app_2_13",
        "@maven//:com_twitter_util_app_lifecycle_2_13",
        "@maven//:com_twitter_util_core_2_13",
        "@maven//:com_twitter_util_slf4j_api_2_13",
        "@maven//:com_twitter_util_slf4j_jul_bridge_2_13",
        "@maven//:io_opentelemetry_opentelemetry_api",
        "@maven//:io_opentelemetry_opentelemetry_context",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_2_13",
        "@maven//:org_slf4j_slf4j_api",
    ],
)

[
    advent_of_code_binary(2022, day)
    for day in range(1, 26)
]

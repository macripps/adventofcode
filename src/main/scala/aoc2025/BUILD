load("@rules_scala//scala:scala.bzl", "scala_library")
load("//tools/build_rules:aoc.bzl", "advent_of_code_binary")

scala_library(
    name = "aoc2025",
    srcs = glob(["*.scala"]),
    visibility = [
        "//visibility:public",
    ],
    deps = [
        "//src/main/scala/aoc",
        #        "@z3//:z3",
        "@maven//:com_github_ben_manes_caffeine_caffeine",
        "@maven//:com_google_inject_guice",
        "@maven//:com_twitter_inject_app_2_13",
        "@maven//:com_twitter_inject_core_2_13",
        "@maven//:com_twitter_util_app_2_13",
        "@maven//:com_twitter_util_app_lifecycle_2_13",
        "@maven//:com_twitter_util_core_2_13",
        "@maven//:com_twitter_util_slf4j_api_2_13",
        "@maven//:com_twitter_util_slf4j_jul_bridge_2_13",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_2_13",
        "@maven//:org_slf4j_slf4j_api",
    ],
)

[
    advent_of_code_binary(2025, day)
    for day in [
        1,
        2,
        3,
        4
    ]
]

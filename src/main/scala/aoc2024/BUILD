load("//tools/build_rules:aoc.bzl", "advent_of_code_binary")
load("@rules_scala//scala:scala.bzl", "scala_library")

scala_library(
    name = "aoc2024",
    srcs = glob(["*.scala"]),
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
    visibility = [
        "//visibility:public",
    ],
)

[
    advent_of_code_binary(2024, day) for day in [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
]

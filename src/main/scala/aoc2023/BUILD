scala_library(
    name = "aoc2023",
    srcs = glob(["*.scala"], exclude=["Day6.scala", "Day9.scala", "Day10.scala", "Day11.scala", "Day12.scala", "Day13.scala"]),
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
        "@maven//:org_slf4j_slf4j_api",
    ],
    visibility = [
        "//visibility:public",
    ],
    resources = [
        "//src/main/resources/aoc2023:day1.input",
        "//src/main/resources/aoc2023:day2.input",
        "//src/main/resources/aoc2023:day3.input",
        "//src/main/resources/aoc2023:day4.input",
        "//src/main/resources/aoc2023:day5.input",
        "//src/main/resources/aoc2023:day8.input",
    ]
)

[
    advent_of_code_binary(2023, day) for day in [6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
]

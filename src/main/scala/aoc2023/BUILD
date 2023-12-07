scala_library(
    name = "aoc2023",
    srcs = glob(["*.scala"], exclude=["Day6.scala"]),
    deps = [
        "//src/main/scala/aoc",
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
        "//src/main/resources/aoc2023:day7.input",
    ]
)

scala_binary(
    name = 'Day6',
    main_class = "aoc2023.Day6Main",
    srcs = [
        "Day6.scala",
    ],
    deps = [
        "//src/main/scala/aoc",
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
    resources = [
        "//src/main/resources/aoc2023:day6.input",
    ]
)

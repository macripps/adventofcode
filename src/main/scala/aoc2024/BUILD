scala_library(
    name = "aoc2024",
    srcs = glob(["*.scala"]),
    deps = [
        "//src/main/scala/aoc",
    ],
    visibility = [
        "//visibility:public",
    ],
    resources = [
        "//src/main/resources/aoc2024:day1.input",
        "//src/main/resources/aoc2024:day2.input",
        "//src/main/resources/aoc2024:day3.input",
        "//src/main/resources/aoc2024:day4.input",
    ]
)

[
    advent_of_code_binary(2024, day) for day in [1,2,3, 4, 6,7]
]

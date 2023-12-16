scala_library(
    name = "aoc2023",
    srcs = glob(["*.scala"], exclude=["Day6.scala", "Day9.scala", "Day10.scala", "Day11.scala", "Day12.scala", "Day13.scala"]),
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
        "//src/main/resources/aoc2023:day8.input",
        "//src/main/resources/aoc2023:day9.input",
    ]
)

[
    advent_of_code_binary(2023, day) for day in [6,9,10,11,12,13,14,15,16]
]

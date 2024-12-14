load("@rules_java//java:defs.bzl", "java_binary")

def advent_of_code_binary(year, day):
    java_binary(
        name = "Day%s" % day,
        main_class = "aoc%s.Day%sMain" % (year, day),
        runtime_deps = [
            "//src/main/scala/aoc%s" % year,
            "@maven//:ch_qos_logback_logback_classic",
        ],
#        data = ["@z3//:dylib"],
        resources = [
            "//src/main/resources/aoc%s:day%s.input" % (year, day),
        ]
    )

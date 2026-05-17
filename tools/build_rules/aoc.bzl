load("@rules_java//java:defs.bzl", "java_binary", "java_library")
load("@rules_scala//scala:scala.bzl", "scala_test")

def advent_of_code_binary(year, day, resources = None):
    if resources == None:
        native.genrule(
            name = "Day%s_input" % day,
            srcs = ["@aoc_inputs_%s//:day%s.input" % (year, day)],
            outs = ["resources/day%s.input" % day],
            cmd = "cp $< $@",
        )
        # Wrap in a java_library with explicit resource_strip_prefix so the
        # resource lands at day{n}.input at the classpath root (not under the
        # full package path that Bazel would otherwise produce).
        java_library(
            name = "Day%s_input_lib" % day,
            resources = [":Day%s_input" % day],
            resource_strip_prefix = native.package_name() + "/resources",
        )
        extra_runtime_deps = [":Day%s_input_lib" % day]
    else:
        extra_runtime_deps = []

    java_binary(
        name = "Day%s" % day,
        main_class = "aoc%s.Day%sMain" % (year, day),
        runtime_deps = [
            "//src/main/scala/aoc%s" % year,
            "@maven//:ch_qos_logback_logback_classic",
        ] + extra_runtime_deps,
        env = {},
#        data = ["@z3//:dylib"],
        resources = resources if resources != None else [],
    )

    native.genrule(
        name = "Day%s_suite_gen" % day,
        outs = ["Day%sSuite.scala" % day],
        cmd = "echo 'package aoc%d' > $@ && echo 'class Day%dSuite extends aoc.AdventDSLSuite(\"aoc%d.Day%d\")' >> $@" % (year, day, year, day),
    )
    scala_test(
        name = "Day%sTest" % day,
        srcs = [":Day%s_suite_gen" % day],
        deps = [
            "//src/test/scala/aoc:advent_dsl_suite",
        ],
        runtime_deps = [
            "//src/main/scala/aoc%s" % year,
            "@maven//:ch_qos_logback_logback_classic",
        ],
    )

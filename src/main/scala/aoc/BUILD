scala_library(
    name = "aoc",
    srcs = [
        "package.scala",
        "Data.scala",
        "Maths.scala",
        "Search.scala",
        "Strings.scala",
    ],
    deps = [
    ],
    visibility = [
        "//visibility:public",
    ],
)

scala_binary(
    name = "AdventOfCode",
    srcs = [
        "AdventOfCode.scala",
    ],
    main_class = "aoc.AdventOfCode",
    deps = [
        ":aoc",
        "//src/main/scala/aoc2022",
        "//src/main/scala/aoc2023",
        "@maven//:com_google_inject_guice",
        "@maven//:com_twitter_inject_app_2_13",
        "@maven//:com_twitter_inject_core_2_13",
        "@maven//:com_twitter_util_app_2_13",
        "@maven//:com_twitter_util_app_lifecycle_2_13",
        "@maven//:com_twitter_util_core_2_13",
        "@maven//:com_twitter_util_logging_2_13",
        "@maven//:com_twitter_util_slf4j_api_2_13",
        "@maven//:com_twitter_util_slf4j_jul_bridge_2_13",
        "@maven//:io_opentelemetry_opentelemetry_api",
        "@maven//:io_opentelemetry_opentelemetry_context",
        "@maven//:io_opentelemetry_opentelemetry_sdk",
        "@maven//:io_opentelemetry_opentelemetry_sdk_common",
        "@maven//:io_opentelemetry_opentelemetry_sdk_trace",
        "@maven//:io_opentelemetry_opentelemetry_exporter_prometheus",
        "@maven//:io_opentelemetry_opentelemetry_exporter_zipkin",
        "@maven//:io_opentelemetry_opentelemetry_semconv",
        "@maven//:org_slf4j_slf4j_api",
    ],
    runtime_deps = [
        "@maven//:ch_qos_logback_logback_classic",
    ],
)

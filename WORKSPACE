load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# See https://github.com/bazelbuild/rules_scala/releases for up to date version information.
rules_scala_version = "6.6.0"
http_archive(
    name = "io_bazel_rules_scala",
    sha256 = "e734eef95cf26c0171566bdc24d83bd82bdaf8ca7873bec6ce9b0d524bdaf05d",
    strip_prefix = "rules_scala-%s" % rules_scala_version,
    url = "https://github.com/bazelbuild/rules_scala/releases/download/v%s/rules_scala-v%s.tar.gz" % (rules_scala_version, rules_scala_version),
)

load("@io_bazel_rules_scala//:scala_config.bzl", "scala_config")
# Stores Scala version and other configuration
# 2.12 is a default version, other versions can be use by passing them explicitly:
# scala_config(scala_version = "2.11.12")
# Scala 3 requires extras...
#   3.2 should be supported on master. Please note that Scala artifacts for version (3.2.2) are not defined in
#   Rules Scala, they need to be provided by your WORKSPACE. You can use external loader like
#   https://github.com/bazelbuild/rules_jvm_external
scala_config(scala_version = "2.13.12", enable_compiler_dependency_tracking = True)

load("@io_bazel_rules_scala//scala:scala.bzl", "rules_scala_setup", "rules_scala_toolchain_deps_repositories")

# loads other rules Rules Scala depends on
rules_scala_setup()

# Loads Maven deps like Scala compiler and standard libs. On production projects you should consider
# defining a custom deps toolchains to use your project libs instead
rules_scala_toolchain_deps_repositories(fetch_sources = True)

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

# optional: setup ScalaTest toolchain and dependencies
load("@io_bazel_rules_scala//testing:scalatest.bzl", "scalatest_repositories", "scalatest_toolchain")
scalatest_repositories()
scalatest_toolchain()

# Z3
_Z3_BUILD = """\
java_import(
    name = "z3",
    jars = [ "bin/com.microsoft.z3.jar" ],
    deps = [
        ":dylib",
    ],
    visibility = ["//visibility:public"],
)

cc_import(
    name = "libz3",
    shared_library = "bin/libz3.dylib",
)

cc_import(
    name = "libz3java",
    shared_library = "bin/libz3java.dylib",
)

cc_library(
    name = "dylib",
    deps = [
      ":libz3",
      ":libz3java",
    ],
    visibility = ["//visibility:public"],
)
"""

http_archive(
  name = "z3",
  build_file_content = _Z3_BUILD,
  urls = ["https://github.com/Z3Prover/z3/releases/download/z3-4.13.3/z3-4.13.3-x64-osx-13.7.zip"],
  integrity = "sha256-jUEDhUwLelmieuIG1dDw/KSLZdu+T9HBPu05bXXaCS8=",
  strip_prefix = "z3-4.13.3-x64-osx-13.7",
)

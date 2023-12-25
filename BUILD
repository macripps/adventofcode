load("@rules_foreign_cc//foreign_cc:defs.bzl", "configure_make", "cmake")

# Executes CMAKE to build Z3 shared libraries.
# If Z3 is updated, the version numbers in the output libraries will need to also
# be updated.
java_library(
    name = "z3",
    srcs = ["@z3//:java_srcs"],
    # Note: Will need to update version numbers when Z3 is updated.
#    out_shared_libs = ["libz3.so", "libz3.so.4.12", "libz3.so.4.12.4.0"],
    visibility = ["//visibility:public"],
)

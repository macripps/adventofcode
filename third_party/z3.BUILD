load("@rules_foreign_cc//foreign_cc:defs.bzl", "cmake")

genrule(
    name = "generated",
    tools = [
        "scripts/mk_make.py",
    ],
    srcs = glob(["**"]),
    outs = [
        "Z3_ast_kind.java",
        "Z3_ast_print_mode.java",
        "Z3_decl_kind.java",
        "Z3_error_code.java",
        "Z3_goal_prec.java",
        "Z3_lbool.java",
        "Z3_param_kind.java",
        "Z3_parameter_kind.java",
        "Z3_sort_kind.java",
        "Z3_symbol_kind.java",
        "Native.java",
    ],
    cmd = "cd external/z3; python3 scripts/mk_make.py --java;cd ../../;" +
     "cp external/z3/src/api/java/Native.java $(location Native.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_ast_kind.java $(location Z3_ast_kind.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_ast_print_mode.java $(location Z3_ast_print_mode.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_decl_kind.java $(location Z3_decl_kind.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_error_code.java $(location Z3_error_code.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_goal_prec.java $(location Z3_goal_prec.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_lbool.java $(location Z3_lbool.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_param_kind.java $(location Z3_param_kind.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_parameter_kind.java $(location Z3_parameter_kind.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_sort_kind.java $(location Z3_sort_kind.java);" +
     "cp external/z3/src/api/java/enumerations/Z3_symbol_kind.java $(location Z3_symbol_kind.java);",
)

java_library(
    name = "enumerations",
    srcs = [
        "Z3_ast_kind.java",
        "Z3_ast_print_mode.java",
        "Z3_decl_kind.java",
        "Z3_error_code.java",
        "Z3_goal_prec.java",
        "Z3_lbool.java",
        "Z3_param_kind.java",
        "Z3_parameter_kind.java",
        "Z3_sort_kind.java",
        "Z3_symbol_kind.java",
    ],
    deps = [":generated"],
)
java_library(
    name = "exception",
    srcs = ["src/api/java/Z3Exception.java"],
)
java_library(
    name = "genjava",
    srcs = ["Native.java",],
    deps = [":generated", ":enumerations", ":exception"],
)
java_library(
    name = "z3",
    srcs = glob(["src/api/java/*.java"]),
    deps = [
        ":enumerations",
        ":exception",
        ":generated",
        ":genjava",
    ],
    javacopts = [
        "-XepDisableAllChecks",
    ],
)

filegroup(
    name = "all_srcs",
    srcs = glob(["**"]),
)

cmake(
    name = "z3_cmake",
    lib_source = ":all_srcs",
    # Note: Will need to update version numbers when Z3 is updated.
    env = {
        "Z3_BUILD_JAVA_BINDINGS": "true"
    },
    out_shared_libs = ["libz3.dylib", "libz3java.dylib"], # .so.4.12", "libz3.so.4.12.2.0"],
    visibility = ["//visibility:public"],
)

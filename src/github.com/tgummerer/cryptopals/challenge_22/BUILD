load("@io_bazel_rules_go//go:def.bzl", "go_library")
load("@io_bazel_rules_go//go:def.bzl", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["challenge_22.go"],
    deps = [
        "//challenge_21:go_default_library"
   ],
    visibility = [
        "//visibility:public"
    ]
)

go_test(
    name = "challenge_22_test",
    srcs = ["challenge_22_test.go"],
    library = ":go_default_library",
    timeout = "short"
)

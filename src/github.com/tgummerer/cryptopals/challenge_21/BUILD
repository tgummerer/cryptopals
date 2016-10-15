load("@io_bazel_rules_go//go:def.bzl", "go_library")
load("@io_bazel_rules_go//go:def.bzl", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["challenge_21.go"],
    visibility = [
        "//visibility:public"
    ]
)

go_test(
    name = "challenge_21_test",
    srcs = ["challenge_21_test.go"],
    library = ":go_default_library",
    timeout = "short"
)

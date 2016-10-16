load("@io_bazel_rules_go//go:def.bzl", "go_library")
load("@io_bazel_rules_go//go:def.bzl", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["challenge_24.go"],
    deps = [
        "//challenge_21:go_default_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)

go_test(
    name = "challenge_24_test",
    srcs = ["challenge_24_test.go"],
    library = ":go_default_library",
    timeout = "short"
)

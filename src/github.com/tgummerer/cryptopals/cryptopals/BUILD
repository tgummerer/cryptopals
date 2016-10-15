load("@io_bazel_rules_go//go:def.bzl", "go_prefix")

go_prefix("github.com/tgummerer/cryptopals")

load("@io_bazel_rules_go//go:def.bzl", "go_library")
load("@io_bazel_rules_go//go:def.bzl", "go_test")

go_library(
    name = "challenge_17_lib",
    srcs = ["challenge_17.go"]
)

go_test(
    name = "challenge_17_test",
    srcs = ["challenge_17_test.go"],
    library = ":challenge_17_lib",
    timeout = "short"
)

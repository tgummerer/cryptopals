load("@io_bazel_rules_go//go:def.bzl", "go_prefix")

go_prefix("github.com/tgummerer/cryptopals")

load("@io_bazel_rules_go//go:def.bzl", "go_binary")
load("@io_bazel_rules_go//go:def.bzl", "go_library")
load("@io_bazel_rules_go//go:def.bzl", "go_test")

go_binary(
    name = "main",
    srcs = [
        "main.go",
    ],
    deps = [
        "//challenge_21:go_default_library",
	"//challenge_22:go_default_library"
    ]
)

go_library(
    name = "ecb",
    srcs = ["ecb.go"],
)

test_fun <- function() {
    lifeCycle(newfun = "new_test", package = "BiocBaseUtils")
}

expect_warning(
    test_fun(),
    "'test_fun' is deprecated\\."
)

expect_warning(
    test_fun(),
    "Use 'new_test' instead\\."
)

expect_warning(
    test_fun(),
    "See help\\('BiocBaseUtils-deprecated'\\)\\."
)

test_fun <- function() {
    lifeCycle(
        newfun = "new_test", package = "BiocBaseUtils", cycle = "defunct"
    )
}

expect_error(
    test_fun(),
    "'test_fun' is defunct\\."
)

expect_error(
    test_fun(),
    "Use 'new_test' instead\\."
)

expect_error(
    test_fun(),
    "See help\\('BiocBaseUtils-defunct'\\)\\."
)

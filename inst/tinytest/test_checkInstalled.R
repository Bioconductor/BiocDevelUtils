expect_error(
    checkInstalled("abc123xyzjfk"),
    "To use this function, install missing package dependencies."
)

tinytest::expect_match(
    trimws(
        capture.output(
            tryCatch({
                checkInstalled("abc123xyzjfk")
            }, error = function(e) {
                invisible()
            })
        )[3]
    ),
    "BiocManager::install\\(\\\"abc123xyzjfk\\\"\\)"
)

expect_true(
    checkInstalled(c("base", "datasets"))
)

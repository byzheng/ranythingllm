test_that("test options", {
    options <- any_options(host = "http://127.0.0.1:3002")
    expect_equal(as.character(options$host), "http://127.0.0.1:3002")

    any_reset()
    options <- any_options()

    expect_equal(as.character(options$host), "http://127.0.0.1:3001/")

})


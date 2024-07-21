test_that("document", {
    skip_if(!is_test_anything())


    test_worksapce <- "ZyVtIHzasdfdsdrRIdHYzhUnwwwoQlDvIupQb"

    # Create workspace
    expect_error(ws <- workspace_new(1))
    expect_error(ws <- workspace_new(c("A", "B")))
    ws <- workspace_new(test_worksapce)
    expect_equal(ws$workspace$name, test_worksapce)
    expect_equal(ws$workspace$slug, tolower(test_worksapce))
    expect_error(ws <- workspace_new(test_worksapce))

    # Delete workspace
    expect_error(ws <- workspace_delete(1))
    expect_error(ws <- workspace_delete(c("A", "B")))
    ws <- workspace_delete(tolower(test_worksapce))
    expect_equal(ws, "OK")
    expect_error(ws <- workspace_delete(tolower(test_worksapce)))
})



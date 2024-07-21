test_that("document", {
    skip_if(!is_test_anything())

    # Raw text
    title <- "dhkodshdsfoirfkldsnvDSHdsfsdhkHKHIdsfkhsk"
    text <- "This is the raw text that will be saved as a document in AnythingLLM."
    new_doc <- documents_raw_text(title, text, docAuthor = "Doc Author",
                                  description = "This is a raw text",
                                  docSource = "from test")
    new_title <- paste0(tolower(title), ".txt")
    #new_doc$documents[[1]]$id
    expect_equal(new_doc$documents[[1]]$title, new_title)
    expect_equal(new_doc$documents[[1]]$docAuthor, "Doc Author")
    new_name <- paste0("raw-", tolower(title), "-", new_doc$documents[[1]]$id, ".json")
    new_doc2 <- document(new_name)
    expect_equal(new_doc2$document$name, new_name)
    expect_equal(new_doc2$document$id, new_doc$documents[[1]]$id)
    remove_name <- paste0("custom-documents/", new_name)
    rsp <- document_remove(remove_name)
    expect_equal(rsp, TRUE)
})



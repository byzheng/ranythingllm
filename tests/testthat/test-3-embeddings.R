test_that("document", {
    skip_if(!is_test_anything())


    test_worksapce <- "ZyVtIHzadrRIdHYzhUnwwwoQlDvIupQbsdfhkdfhksHKD"

    title <- "dhkodshdsfoirfkldsnvDSsssadfsafdsfHJHGIdsfdsfskHK"
    text <- "This is the raw text that will be saved as a document in AnythingLLM."

    ws <- workspace_new(test_worksapce)
    # Move files
    new_doc <- documents_raw_text(title, text, docAuthor = "Doc Author",
                                  description = "This is a raw text",
                                  docSource = "from test")
    new_doc2 <- documents_raw_text(title, text, docAuthor = "Doc Author",
                                   description = "This is a raw text",
                                   docSource = "from test")

    files <- c(paste0("custom-documents/raw-", tolower(title), "-", new_doc$documents[[1]]$id, ".json"),
               paste0("custom-documents/raw-", tolower(title), "-", new_doc2$documents[[1]]$id, ".json"))

    resp <- embeddings(test_worksapce, adds = files)
    resp <- embeddings(test_worksapce, deletes = files)
    resp <- document_remove(files)
    ws <- workspace_delete(tolower(test_worksapce))

})



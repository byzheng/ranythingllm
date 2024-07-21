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

    test_folder <- "test_folder"
    #create_folder(test_folder)

    # Move files
    new_doc <- documents_raw_text(title, text, docAuthor = "Doc Author",
                                  description = "This is a raw text",
                                  docSource = "from test")
    new_doc2 <- documents_raw_text(title, text, docAuthor = "Doc Author",
                                  description = "This is a raw text",
                                  docSource = "from test")
    files <- c(paste0("custom-documents/raw-", tolower(title), "-", new_doc$documents[[1]]$id, ".json"),
               paste0("custom-documents/raw-", tolower(title), "-", new_doc2$documents[[1]]$id, ".json"))

    move_files(files, test_folder)

    new_files <- file.path(test_folder, basename(files))
    doc1 <- document(basename(new_files[1]))
    expect_equal(doc1$document$name, basename(files[1]))
    doc2 <- document(basename(new_files[2]))
    expect_equal(doc2$document$name, basename(files[2]))
    document_remove(new_files[1])
    document_remove(new_files[2])
})



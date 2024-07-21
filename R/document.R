# function related with document

#' List of all locally-stored documents in instance
#'
#' @return A list of all documents
#' @export
documents <- function() {
    request(path = "documents")
}

#' Get a single document by its unique AnythingLLM document name
#'
#' @param name Unique document name to find (name in /documents)
#'
#' @return A list for document
#' @export
document <- function(name) {
    stopifnot(length(name) == 1)
    stopifnot(is.character(name))
    request(method = "GET", path = file.path("document", name))
}




#' Upload a file by specifying its raw text content and metadata values without having to upload a file.
#'
#' @param title file title
#' @param text raw text of file
#' @param ... other metadata with key and values
#'
#' @return The document metadata
#' @export
documents_raw_text <- function(title, text, ...) {
    stopifnot(length(title) == 1)
    stopifnot(is.character(title))
    stopifnot(length(text) == 1)
    stopifnot(is.character(text))
    meta <- list(...)
    if (sum(nchar(names(meta)) == 0) > 0) {
        stop("All metadata should have a name.")
    }
    meta$title <- title
    body <- list(textContent = text,
                 metadata = meta,
                type = "application/json")
    resp <- request(method = "POST", path = "document/raw-text",
                    body = body)
    resp
}


#' Permanently remove documents from the system.
#'
#' @description
#' Folder name is required to remove a document
#' @param names Array of document names to be removed permanently.
#'
#' @return TRUE if success
#' @export
document_remove <- function(names) {
    #stop("There is a bug in the Anything LLM API. Document cannot be removed at this stage")
    stopifnot(is.vector(names))
    stopifnot(sum(!is.character(names)) == 0)

    body <- list(names = names,
                 type = "application/json")
    resp <- request(method = "DELETE", path = "system/remove-documents",
                    body = body,
                    auto_unbox = FALSE)
    return(TRUE)
}


#' Create a new folder inside the documents storage directory.
#'
#' @param name folder name
#'
#' @return TRUE if folder is created
#' @export
create_folder <- function(name) {
    stopifnot(length(name) == 1)
    stopifnot(is.character(name))
    path <- "document/create-folder"
    body <- list(name = name,
                 type = "application/json")
    resp <- request(method = "POST", path = path,
                    body = body,
                    auto_unbox = TRUE)
    return(TRUE)
}

#' Move files within the documents storage directory.
#'
#' @param files files to move including the folder name
#' @param folder new folder name
#'
#' @return TRUE if files are moved into new folder
#' @export
move_files <- function(files, folder) {
    stopifnot(is.vector(files))
    stopifnot(sum(!is.character(files)) == 0)
    stopifnot(length(folder) == 1)
    stopifnot(is.character(folder))
    path <- "/document/move-files"
    body <- list(type = "application/json",
                 files = list())
    for (i in seq(along = files)) {
        new_file <- file.path(folder, basename(files[i]))
        body$files[[i]] <- list(from = files[i],
                                to = new_file)
    }
    resp <- request(method = "POST", path = path,
                    body = body,
                    auto_unbox = TRUE)
    return(TRUE)
}

documents_upload <- function(file) {
    stopifnot(length(file) == 1)
    stopifnot(is.character(file))
    stopifnot(file.exists(file))

    resp <- request(method = "POST", path = "document/upload",
                    body = list(method = "req_body_multipart",
                                file = file,
                                type = "application/pdf"))
    resp
}


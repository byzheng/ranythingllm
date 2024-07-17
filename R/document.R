# function related with document

documents <- function() {
    request(path = "documents")
}

documents_upload <- function(file) {
    stopifnot(length(file) == 1)
    stopifnot(is.character(file))
    stopifnot(file.exists(file))

    request(method = "PUT", path = "document/upload",
            body = list(method = "req_body_multipart",
                        file = file,
                        type = "application/pdf"))
}

# function related with authentication
auth <- function() {
    request(path = "auth")
}

w_add_embeddings <- function(slug, adds = NULL, deletes = NULL) {
    stopifnot(length(slug) == 1)
    stopifnot(is.character(slug))

    if (is.null(adds) && is.null(deletes)) {
        stop("One of adds and deletes should be specified")
    }
    body <- list(type = "application/json")
    if (!is.null(adds)) {
        stopifnot(is.vector(adds))
        stopifnot(is.character(adds))
        body$adds <- adds
    }
    if (!is.null(deletes)) {
        stopifnot(is.vector(deletes))
        stopifnot(is.character(deletes))
        body$deletes <- deletes
    }
    path <- sprintf("workspace/%s/update-embeddings", slug)

    resp <- request(method = "POST", path = path,
                    body = body)
    resp
    # {
    #     "adds": [
    #         "custom-documents/my-pdf.pdf-hash.json"
    #     ],
    #     "deletes": [
    #         "custom-documents/anythingllm.txt-hash.json"
    #     ]
    # }
}




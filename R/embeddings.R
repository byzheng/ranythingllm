# function related with embeddings

#' Add or remove documents from a workspace by its unique slug.
#'
#' @param slug Unique slug of workspace to find
#' @param adds files to add
#' @param deletes files to delete
#'
#' @return TRUE if embeddings updated
#' @export
embeddings <- function(slug, adds = NULL, deletes = NULL) {
    stopifnot(length(slug) == 1)
    stopifnot(is.character(slug))
    slug <- tolower(slug)

    if (is.null(adds) && is.null(deletes)) {
        stop("One of adds and deletes should be specified")
    }
    body <- list(type = "application/json",
                 adds = character(0),
                 deletes = character(0))

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
                    body = body,
                    auto_unbox = FALSE)
    resp
}


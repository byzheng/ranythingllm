


#' Perform a request to TiddlyWiki WebServer
#'
#' @param method The method in the httr package, e.g. GET, POST
#' @param path The path of request
#' @param query The query of request
#' @param ... Other arguments of request
#'
#' @return The contents of response
request <- function(method = "GET",
                           path = '/',
                           query = list(),
                           ...) {
    stopifnot(length(method) == 1)
    stopifnot(is.character(method))
    stopifnot(method %in% c("GET", "PUT", "POST", "DELETE"))

    host <- ANY_OPTIONS("host")
    stopifnot(length(host) == 1)
    apikey <- ANY_OPTIONS("apikey")
    stopifnot(length(apikey) == 1)
    if (apikey == "") {
        stop("Not an valid API key")
    }

    req <- httr2::request(host) |>
        httr2::req_url_path_append("/api/v1") |>
        httr2::req_url_path_append(path) |>
        httr2::req_method(method)

    resp <- req |>
        httr2::req_auth_bearer_token(apikey) |>
        httr2::req_perform()

    resp |>
        httr2::resp_body_json()

}


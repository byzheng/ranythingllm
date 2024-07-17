


#' Perform a request to TiddlyWiki WebServer
#'
#' @param method The method in the httr package, e.g. GET, POST
#' @param path The path of request
#' @param query The query of request
#' @param body the query body
#'
#' @return The contents of response
request <- function(method = "GET",
                    path = '/',
                    body = NULL,
                    query = list()) {
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

    if (!is.null(body)) {
        if (body$method == "req_body_multipart") {
            req <- req |>
                httr2::req_body_multipart(file = curl::form_file(body$file),
                                          type = body$type)
        }
    }
    resp <- req |>
        httr2::req_auth_bearer_token(apikey) |>
        httr2::req_perform()

    resp |>
        httr2::resp_body_json()

}


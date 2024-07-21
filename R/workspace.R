# function related with workspace


#' List all current workspaces
#'
#' @return A list of current workspaces
#' @export
workspaces <- function() {
    ws <- request(path = "workspaces")
    ws
}


#' Get a workspace by its unique slug.
#'
#' @param slug workspace slug
#'
#' @return A list for workspace
#' @export
workspace <- function(slug) {
    stopifnot(length(slug) == 1)
    stopifnot(is.character(slug))
    path <- sprintf("/workspace/%s", slug)
    ws <- request(method = "GET", path = path)
    ws
}

#' Create a new workspace
#'
#' @param name workspace name
#'
#' @return A list for new workspace
#' @export
workspace_new <- function(name) {
    stopifnot(length(name) == 1)
    stopifnot(is.character(name))

    ws <- workspaces()
    ws_check <- ws$workspaces |>
        purrr::keep(function(x) x$name == name)
    if (length(ws_check) > 0) {
        stop("Exist workspace for ", name)
    }

    body <- list(name = name,
                 type = "application/json")
    ws <- request(method = "POST", path = "workspace/new", body = body)
    ws
}




#' Deletes a workspace by its slug.
#'
#' @param slug workspace slug
#'
#' @return OK if deleted
#' @export
workspace_delete <- function(slug) {
    stopifnot(length(slug) == 1)
    stopifnot(is.character(slug))

    ws <- workspaces()
    ws_check <- ws$workspaces |>
        purrr::keep(function(x) x$slug == slug)
    if (length(ws_check) == 0) {
        stop("Cannot find workspace for ", slug)
    }
    path <- sprintf("/workspace/%s", slug)
    ws <- request(method = "DELETE", path = path)
    ws
}


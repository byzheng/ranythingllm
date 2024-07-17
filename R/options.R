# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
ANY_OPTIONS <- settings::options_manager(
    host = "http://127.0.0.1:3001/",
    apikey = Sys.getenv("ANYTHINGLLM_APIKEY")
)


#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#'  host: Host of anything llm
#'  apikey: apikey of anything llm
#'
#' @return the default and modified options.
#' @export
#' @examples
#' any_options(host = "http://127.0.0.1:3001/")
any_options <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    ANY_OPTIONS(...)
}

#' Reset global options for pkg
#'
#' @return the default options
#' @export
#' @examples
#' any_reset()
any_reset <- function() {
    settings::reset(ANY_OPTIONS)
}

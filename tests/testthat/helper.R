
#' Test whether server can be connected
#'
#' @return TRUE if test server is available.
is_test_anything <- function() {
    tryCatch({
        status <- auth()
        if (status$authenticated == TRUE) {
            return(TRUE)
        }
    }, error = function(e){
        return (FALSE)
    })
    return(FALSE)
}


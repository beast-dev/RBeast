#' Determines if the input is a BEAST2 posterior
#' @param x the input
#' @param verbose give verbose output, should be TRUE or FALSE
#' @return TRUE or FALSE
#' @author Richel Bilderbeek
#' @examples
#'   trees_filename <- system.file(
#'    "extdata", "beast2_example_output.trees", package = "RBeast"
#'   )
#'   log_filename <- system.file(
#'    "extdata", "beast2_example_output.log", package = "RBeast"
#'   )
#'   posterior <- parse_beast_posterior(
#'     trees_filename = trees_filename,
#'     log_filename = log_filename
#'   )
#'   testit::assert(is_posterior(posterior))
#' @export
is_posterior <- function(
  x,
  verbose = FALSE
) {
  if (verbose != TRUE && verbose != FALSE) {
    stop(
      "is_posterior: verbose should be TRUE or FALSE"
    )
  }
  if (class(x) != "list") {
    if (verbose) {
      message("is_posterior: x is not a list")
    }
    return(FALSE)
  }

  return(TRUE)
}

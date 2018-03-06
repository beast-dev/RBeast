#' Determines if the input is a BEAST2 posterior
#' @param x the input
#' @return TRUE or FALSE
#' @author Richel J.C. Bilderbeek
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
is_posterior <- function(x) {

  tracerer::is_posterior(x)
}

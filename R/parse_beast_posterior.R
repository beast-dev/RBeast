#' Parses BEAST2 output files to a posterior
#' @param trees_filename name of the BEAST2 .trees output file
#' @param log_filename name of the BEAST2 .trees output file
#' @return a posterior
#' @export
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
#' @author Richel J.C. Bilderbeek
parse_beast_posterior <- function(trees_filename, log_filename) {

  beastier::parse_beast_posterior(trees_filename, log_filename)
}

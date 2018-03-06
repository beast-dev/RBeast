#' Parses a BEAST2 .trees output file
#' @param filename name of the BEAST2 .trees output file
#' @return the phylogenies in the posterior
#' @export
#' @examples
#'   trees_filename <- system.file(
#'     "extdata", "beast2_example_output.trees", package = "RBeast"
#'   )
#'   posterior <- parse_beast_trees(
#'     filename = trees_filename
#'   )
#'   testit::assert(is_trees_posterior(posterior))
#' @author Richel J.C. Bilderbeek
parse_beast_trees <- function(filename) {

  tracerer::parse_beast_trees(filename)
}

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
#' @note The function is not yet able to respond
#'   correctly to invalid files, as documented at
#'   https://github.com/richelbilderbeek/Cer2016/issues/118
#' @author Richel Bilderbeek
parse_beast_trees <- function(filename) {

  if (!file.exists(filename)) {
    stop("file absent")
  }

  posterior <- rBEAST::beast2out.read.trees(filename)

  if (length(posterior) == 1 && is.na(posterior)) {
    stop("invalid file")
  }

  posterior
}

#' Extract a list of phylogenies from a BEAST2 posterior file
#' @param filename name of the BEAST2 posterior filename, usually ends with '.trees'
#' @return a list of phylogenies of type 'phylo'
#' @examples
#'   trees_file <- system.file(
#'     "extdata", "read_beast2_trees_example.trees", package = "RBeast"
#'   )
#'   testit::assert(file.exists(trees_file))
#'   posterior <- read_beast2_trees(trees_file)
#'   testit::assert(length(posterior) == 10)
#'   testit::assert(class(posterior[[1]]) == "phylo")
#' @export
#' @author Richel Bilderbeek
read_beast2_trees <- function(filename) {
  if (!file.exists(filename)) {
    stop("read_beast2_trees: file not found")
  }
  return (rBEAST::beast2out.read.trees(filename))
}

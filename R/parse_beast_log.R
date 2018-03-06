#' Parses a BEAST2 .log output file
#' @param filename name of the BEAST2 .log output file
#' @return data frame with all the parameter estimates
#' @export
#' @examples
#'   log_filename <- system.file(
#'     "extdata", "beast2_example_output.log", package = "RBeast"
#'   )
#'   estimates <- parse_beast_log(filename = log_filename)
#'   expected_names <- c(
#'     "Sample", "posterior", "likelihood",
#'     "prior", "treeLikelihood", "TreeHeight",
#'     "BirthDeath", "birthRate2", "relativeDeathRate2"
#'   )
#'   testit::assert(names(estimates) == expected_names)
#' @author Richel J.C. Bilderbeek
parse_beast_log <- function(filename) {
  tracerer::parse_beast_log(filename)
}

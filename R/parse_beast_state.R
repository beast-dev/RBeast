#' Parses a BEAST2 .xml.state output file to get only the operators
#'   acceptances
#' @param filename name of the BEAST2 .xml.state output file
#' @return data frame with all the operators' success rates
#' @export
#' @examples
#'   xml_state_filename <- system.file(
#'     "extdata", "beast2_example_output.xml.state", package = "RBeast"
#'   )
#'   estimates <- parse_beast_state_operators(filename = xml_state_filename)
#'   expected_names <- c("operator", "p", "accept", "reject", "acceptFC",
#'     "rejectFC", "rejectIv", "rejectOp")
#'   expected_operator <- c("treeScaler.t", "treeRootScaler.t",
#'     "UniformOperator.t", "SubtreeSlide.t", "narrow.t", "wide.t",
#'     "WilsonBalding.t", "BirthRateScaler.t", "DeathRateScaler.t")
#'   testit::assert(names(estimates) == expected_names)
#'   #testit::assert(estimates$operator == expected_operators)
#' @author Richel Bilderbeek
parse_beast_state_operators <- function(
    filename = system.file("extdata", "beast2_example_output.xml.state", package = "RBeast")
) {
  if (!file.exists(filename)) {
    stop("file absent")
  }

  lines <- readLines(filename, warn = FALSE)
  stringr::str_match(string = lines,
    pattern = "\\\"id\\\":\\\".*:.*\\\",\\\"p\\\":[:digit:]+.[:digit:]+,\\\"accept\\\":[:digit:]+,\\\"reject\\\":[:digit:]+,\\\"acceptFC\\\":[:digit:]+,\\\"rejectFC\\\":[:digit:]+,\\\"rejectIv\\\":[:digit:]+,\\\"rejectOp\\\":[:digit:]+")


  operators <- data.frame(
    operator = NA,
    p = NA,
    accept = NA,
    reject = NA,
    acceptFC = NA,
    rejectFC = NA,
    rejectIv = NA,
    rejectOp = NA
  )
  operators
}

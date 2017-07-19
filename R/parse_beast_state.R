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

  lines <- RBeast::extract_operators_lines(filename)
  json <- jsonlite::fromJSON(lines)
  df <- as.data.frame(json)
  names(df) <- gsub("operators.", "", names(df))
  testit::assert(names(df)[1] == "id")
  names(df)[1] <- "operator"

  # 'WilsonBalding.t:test-alignment_to_beast_posterior' -> 'WilsonBalding.t'
  df$operator <- gsub(":.*", "", df$operator)

  df
}


#' Extract the JSON lines out of a .xml.state file with the operators
#' @export
#' @author Richel Bilderbeek
extract_operators_lines <- function(filename)
{
  if (!file.exists(filename)) {
    stop("file absent")
  }

  lines <- readLines(filename, warn = FALSE)

  start_indices <- lines ==  "{\"operators\":["
  testit::assert(sum(start_indices) == 1)
  start_index <- which(start_indices == TRUE)
  testit::assert(lines[start_index] == "{\"operators\":[")

  end_indices <- lines ==  "]}"
  testit::assert(sum(end_indices) == 1)
  end_index <- which(end_indices == TRUE)
  testit::assert(lines[end_index] == "]}")

  operator_lines <- lines[start_index:end_index]
  operator_lines
}

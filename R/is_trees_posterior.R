#' Determines if the input is a BEAST2 posterior,
#' as parsed by parse_beast_trees
#' @param x the input
#' @return TRUE or FALSE
#' @author Richel Bilderbeek
#' @export
is_trees_posterior <- function(x) {

  if (class(x) != "list") {
    return(FALSE)
  }
  for (item in x) {
    if (class(item) != "phylo") {
      return(FALSE)
    }
  }

  valid_name_regex <- "^STATE_[[:digit:]]+$"
  valid_names <- grep(valid_name_regex, names(x), perl = TRUE, value = TRUE)
  testit::assert(length(valid_names) == length(x))
  values <- sub("STATE_(\\d+)", "\\1", names(x))
  if (is.unsorted(as.numeric(values))) {
    return(FALSE)
  }
  return(TRUE)
}

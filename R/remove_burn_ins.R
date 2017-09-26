#' Removed the burn-ins from a data frame
#' @param traces a data frame with traces
#' @param burn_in_fraction the fraction that needs to be removed, must be [0,1>
#' @return the data frame with the burn-in removed
#' @export
#' @author Richel Bilderbeek
remove_burn_ins <- function(traces, burn_in_fraction) {
  if (!is.data.frame(traces)) {
    stop("traces must be a data.frame")
  }
  if (burn_in_fraction < 0.0) {
    stop("burn_in_fraction must be at least zero")
  }
  if (burn_in_fraction > 1.0) {
    stop("burn_in_fraction must be at most one")
  }
  n <- nrow(traces)
  first_index <- as.integer(1 + (n * burn_in_fraction))

  if (first_index >= nrow(traces)) {
    return(traces[0, ])
  }
  out <- traces[ seq(first_index, n), ]
  out
}

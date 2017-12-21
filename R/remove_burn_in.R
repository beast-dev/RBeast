#' Removed the burn-in from a trace
#' @param trace the values
#' @param burn_in_fraction the fraction that needs to be removed, must be [0,1>
#' @return the values with the burn-in removed
#' @export
#' @examples
#'   # Create a trace from one to and including ten
#'   v <- seq(1, 10)
#'
#'   # Remove the first ten percent of its values,
#'   # in this case removes the first value, which is one
#'   w <- remove_burn_in(trace = v, burn_in_fraction = 0.1)
#'
#'   # Check that the result goes from two to ten
#'   testit::assert(w == seq(2, 10))
#' @author Richel J.C. Bilderbeek
remove_burn_in <- function(trace, burn_in_fraction) {

  beastier::remove_burn_in(trace, burn_in_fraction)
}

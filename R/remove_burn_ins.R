#' Removed the burn-ins from a data frame
#' @param traces a data frame with traces
#' @param burn_in_fraction the fraction that needs to be removed, must be [0,1>
#' @return the data frame with the burn-in removed
#' @export
#' @author Richel J.C. Bilderbeek
remove_burn_ins <- function(traces, burn_in_fraction) {

  tracerer::remove_burn_ins(traces, burn_in_fraction)
}

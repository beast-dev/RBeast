## ------------------------------------------------------------------------
library(Cer2016)
library(coda)

## ------------------------------------------------------------------------
estimates <- parse_beast_log(
  filename = find_path("beast2_example_output.log")
)

## ------------------------------------------------------------------------
esses <- rep(NA, ncol(estimates))
burn_in_fraction <- 0.1
for (i in seq_along(estimates)) {
  # Trace with the burn-in still present
  trace_raw <- as.numeric(t(estimates[i]))

  # Trace with the burn-in removed
  trace <- remove_burn_in(trace = trace_raw, burn_in_fraction = 0.1)

  # Store the effectice sample size
  esses[i] <- calc_ess(trace, sample_interval = 1000)
}

# Note that the first value of three is nonsense:
# it is the index of the sample. I keep it in
# for simplicity of writing this code
expected_esses <- c(3, 10, 10, 10, 10, 7, 10, 9, 6)
testit::assert(all(expected_esses - esses < 0.5))

df_esses <- data.frame(esses)
rownames(df_esses) <- names(estimates)
knitr::kable(df_esses)

## ----fig.width = 7, fig.height = 7---------------------------------------
melted <- reshape2::melt(estimates, id.vars = c("Sample"))

ggplot2::ggplot(
  melted, 
  ggplot2::aes(
    x = melted$Sample, 
    y = melted$value,
    color = as.factor(melted$variable)
  )
) + ggplot2::geom_line() +
  ggplot2::ggtitle("Parameter estimates")


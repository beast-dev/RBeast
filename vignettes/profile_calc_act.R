## ------------------------------------------------------------------------
library(RBeast)

## ------------------------------------------------------------------------
set.seed(42)
trace <- runif(n = 3200, min = 0.0, max = 1.0)

## ------------------------------------------------------------------------
if (1 == 2) {
rprof_tmp_output <- "~/tmp_RBeast_rprof"
Rprof(rprof_tmp_output)
calc_act(trace = trace, sample_interval = 1)
Rprof(NULL)
summaryRprof(rprof_tmp_output)
}

## ------------------------------------------------------------------------
rprof_tmp_output <- "~/tmp_RBeast_rprof"
Rprof(rprof_tmp_output)
x <- calc_act(trace = trace, sample_interval = 1)
y <- calc_act_r(trace = trace, sample_interval = 1)
Rprof(NULL)
summaryRprof(rprof_tmp_output)
print(x)
print(y)



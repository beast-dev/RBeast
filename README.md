# RBeast

Branch|[![Travis CI logo](TravisCI.png)](https://travis-ci.org)|[![Codecov logo](Codecov.png)](https://www.codecov.io)
---|---|---
master|[![Build Status](https://travis-ci.org/beast-dev/RBeast.svg?branch=master)](https://travis-ci.org/beast-dev/RBeast)|[![codecov.io](https://codecov.io/github/beast-dev/RBeast/coverage.svg?branch=master)](https://codecov.io/github/beast-dev/RBeast/branch/master)
develop|[![Build Status](https://travis-ci.org/beast-dev/RBeast.svg?branch=develop)](https://travis-ci.org/beast-dev/RBeast)|[![codecov.io](https://codecov.io/github/beast-dev/RBeast/coverage.svg?branch=develop)](https://codecov.io/github/beast-dev/RBeast/branch/develop)

R package for working with BEAST and BEAST2.

Use [beautier](https://github.com/richelbilderbeek/beautier) to generate BEAST2 input (`.xml`) files.

Use [beastier](https://github.com/richelbilderbeek/beastier) for calling BEAST2 from R, and parsing BEAST2 output files. 

## Example

```
library(RBeast)

# Obtain an example log file its name
filename <- system.file(
  "extdata", "beast2_example_output.log", package = "RBeast"
)

# Parse that log file
beast_log_full <- parse_beast_log(filename)

# Remove the burn-in
beast_log <- remove_burn_ins(
  beast_log_full,
  burn_in_fraction = 0.1
)

# Calculates the effective sample sizes of all parameter estimates
esses <- calc_esses(beast_log, sample_interval = 1000)
```

## Instructions

To install `RBeast` in `R`:

```{r}
install.packages("devtools")
devtools::install_github("beast-dev/RBeast")
```

## Acknowledgements

 * This project is supported in part through the National Science Foundation grant DMS 1264153.


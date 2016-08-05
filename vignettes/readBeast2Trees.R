## ----message = FALSE-----------------------------------------------------
library(RBeast)

## ------------------------------------------------------------------------
trees_file <- "../inst/extdata/readBeast2TreesExample.trees"
testit::assert(file.exists(trees_file))

## ------------------------------------------------------------------------
posterior_trees <- readBeast2Trees(trees_file)

## ------------------------------------------------------------------------
names(posterior_trees)
testit::assert(length(posterior_trees) == 10)

## ------------------------------------------------------------------------
testit::assert(class(posterior_trees[[1]]) == "phylo")

## ------------------------------------------------------------------------
for (p in posterior_trees) {
  plot(p)
}


## ----message = FALSE-----------------------------------------------------
library(RBeast)

## ------------------------------------------------------------------------
trees_file <- system.file(
	"extdata", "read_beast2_trees_example.trees", package = "RBeast"
)
testit::assert(file.exists(trees_file))

## ------------------------------------------------------------------------

posterior_trees <- read_beast2_trees(trees_file)

## ------------------------------------------------------------------------
names(posterior_trees)
testit::assert(length(posterior_trees) == 11)

## ------------------------------------------------------------------------
testit::assert(class(posterior_trees[[1]]) == "phylo")

## ------------------------------------------------------------------------
for (p in posterior_trees) {
  graphics::plot(p)
}

## ----fig.width = 7, fig.height = 7---------------------------------------
class(posterior_trees) <- "multiPhylo"
phangorn::densiTree(
  posterior_trees,
  type = "cladogram",
  alpha = 1
)

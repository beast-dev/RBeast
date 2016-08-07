context("read_beast2_trees")

test_that("read_beast2_trees: use", {
	trees_file <- system.file(
		"extdata", "read_beast2_trees_example.trees", package = "RBeast"
	)
	expect_true(file.exists(trees_file))
	posterior <- read_beast2_trees(trees_file)
	expect_equal(length(posterior), 10)
	expect_equal(class(posterior[[1]]), "phylo")
})


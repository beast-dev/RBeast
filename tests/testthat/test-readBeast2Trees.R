context("readBeast2Trees")

test_that("readBeast2Trees: use", {
	trees_file <- system.file("extdata", "readBeast2TreesExample.trees", package = "RBeast")
	expect_true(file.exists(trees_file))
	posterior <- readBeast2Trees(trees_file)
	expect_equal(length(posterior), 10)
	expect_equal(class(posterior[[1]]), "phylo")
})


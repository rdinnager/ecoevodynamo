test_that("extract_examples works as expected", {

  ## mac_lev, Ns and traits are created in testthat/helper-eed_solver.R

  ## five ways to specify example_inputs
  test1 <- extract_examples(rlang::quo(mac_lev), list(Ns = Ns, traits = traits))
  test2 <- extract_examples(rlang::quo("mac_lev"), list(Ns = Ns, traits = traits))
  test3 <- extract_examples(rlang::quo(mac_lev(Ns = Ns, traits = traits)))

  expect_identical(test1, test2)
  expect_identical(test1, test3)
  expect_identical(test2, test3)

  expect_s3_class(test1[[2]]$N, "torch_tensor")

  expect_snapshot(test1)

})

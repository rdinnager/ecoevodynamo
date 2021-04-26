test_that("extract_examples works as expected", {

  ## mac_lev, Ns and traits are created in testthat/helper-objects_for_testing.R

  ## three ways to specify example_inputs
  test1 <- eed_solver(mac_lev, list(Ns = Ns, traits = traits))
  test2 <- eed_solver("mac_lev", list(Ns = Ns, traits = traits))
  test3 <- eed_solver(mac_lev(Ns = Ns, traits = traits))

  expect_identical(test1, test2)
  expect_identical(test1, test3)
  expect_identical(test2, test3)

  expect_s3_class(test1$example_inputs$Ns, "torch_tensor")

  expect_snapshot(test1)

})

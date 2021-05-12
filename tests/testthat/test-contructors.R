test_that("eed_eco creates correct object", {

  expect_error(eed_eco({
    x <- torch_cdist(X, X_)
    dN <- N * N_*exp(-x)
    dN
  }))

  expect_error(eed_eco({
    x <- torch_cdist(X, X_)
    dN <- N * N_*exp(-x)
    c(dN = dN)
  }))

  eco <- eed_eco({
    x <- torch_cdist(X, X_)
    dN <- N * N_ * exp(-x)
    list(dN = dN)
  })

  expect_snapshot(eco)

  expect_identical(attr(eco, "returns"), "dN")
  expect_identical(attr(eco, "vars"), c("X", "X_", "N", "N_"))
  expect_identical(attr(eco, "arg_names"), c("X", "X_", "N", "N_"))

})

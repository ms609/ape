test_that("Random coalescent trees", {
  BOUND <- qnorm(0.9995)
  N <- 100
  tree.sizes <- c(5, 10, 20, 50, 75, 100)
  res <- numeric()
  for (i in 1:200) {
    for (n in tree.sizes) {
      k <- 2:n
      expected.mean <- 2 * sum(1/(k * (k - 1)))
      expected.var <- 4 * sum(1/(k * (k - 1))^2)
      x <- replicate(N, branching.times(rcoal(n))[1])
      res <- c(res, (mean(x) - expected.mean) * sqrt(N/expected.var))
    }
    if (anyNA(res)) {
      message("Missing values returned after", length(res), "simulations")
      expect_true(FALSE)
    }
    tab <- tabulate((abs(res) > BOUND) + 1L, 2L)[2]
    expect_lt(tab, length(res) / 100)
  }
})

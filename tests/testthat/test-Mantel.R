test_that("Type I error rate of the Mantel test", {
  N = 100
  n = 10

  rmat <- function(n) {
    x <- runif(n * (n - 1) / 2)
    m <- matrix(0, n, n)
    m[lower.tri(m)] <- x
    m <- t(m)
    m[lower.tri(m)] <- x
    m
  }
  res <- numeric()
  for (i in 1:200) {
    res <- c(res, replicate(N, {
      ma <- rmat(n)
      mb <- rmat(n)
      mantel.test(ma, mb)$p
    }))
    if (anyNA(res)) {
      message("Missing values returned after", length(res), "simulations")
      expect_true(FALSE)
    }
    nsig <- sum(res < 0.05)
  }
  expect_lt(nsig / length(res), 0.05)
})

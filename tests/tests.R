library(testthat)

x <- seq(1, 10)
y <- runif(10, 0, 1)

test_that("The kernel function works as expected", {
    expect_error(
        kernel_smoother(x, y, "epanechnikov", 5))
    expect_error(
        kernel_smoother(x, y, "logistic", 5))
    expect_equal(
        as.vector(kernel_smoother(x, y, "gaussian", 7)),
        ksmooth(x, y, "normal", 7, n.points = 10)$y)
    expect_equal(
        as.vector(kernel_smoother(x, y, "uniform", 7)),
        ksmooth(x, y, "box", 7, n.points = 10)$y)
    }
)
#' @param x Vector of values for the predictor variable
#' @param y Vector of values for the response variable
#' @param method Kernel to smooth the data with
#' @param n_iter Number of iterations to run the CV for

cross_val <- function(x, y, method = c("uniform", "gaussian"), n_iter = 20) {

    # Pre-allocate space
    band <- seq(1, n_iter, length.out = n_iter)
    error <- double(n_iter)

    # Run CV
    for (i in 1:n_iter) {
        fit <- kernel_smoother(x, y, match.arg(method), bandwidth = i)
        error[i] <- mean_sq_err(obs = y, est = fit)
    }
    best_band <- band[which.min(error)]
    results <- list(mse = min(error), bandwidth = best_band)
    return(results)
}
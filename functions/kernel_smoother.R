#' @param x A vector of values for the predictor variable
#' @param y A vector of values for the response variable
#' @param kernel The kernel to smooth the data with
#' @param bandwidth The coverage range of the kernel

kernel_smoother <- function(x, y, kernel = c("gaussian", "uniform"), bandwidth) {
    dist <- as.matrix(dist(x))
    kernel <- match.arg(kernel)

    # Ensure specified kernel is valid
    if (kernel %in% c("gaussian", "uniform") == FALSE) {
        stop("specified kernel must be either gaussian or uniform")
    }

    # Fit kernel regression
    S <- switch(
        kernel,
        gaussian = dnorm(dist, mean = 0, sd = bandwidth * 0.3706506),
        uniform = dist <= (bandwidth * 0.5))
    S <- sweep(S, 1, rowSums(S), "/")
    mod_fit <- S %*% y
    return(mod_fit)
}
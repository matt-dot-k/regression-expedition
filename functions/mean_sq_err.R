#' @param obs A vector of observed values in the data
#' @param est A vector of values obtained from a model prediction

mean_sq_err <- function(obs, est) {
    diff_sq <- (obs - est) ^ 2
    mse <- mean(diff_sq)
    return(mse)
}
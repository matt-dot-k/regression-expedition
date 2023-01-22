#' @param data A two column data frame with columns x and y
#' @param method Whether to fit a GAM or a spline
#' @param df If spline is selected, the number of knots to choose. Defaults to (df - 3) knots.

gam_or_spline <- function(data, method = c("gam", "spline"), df = NULL) {

    # Check for valid inputs
    if (is.data.frame(data) == FALSE) {
        stop("supplied data must be of type 'data.frame'")
    }
    columns <- colnames(data)
    for (i in 1:length(columns)) {
        if (columns[i] %in% c("x", "y") == FALSE) {
            stop("data must have columns named 'x' and 'y'")
        }
    }
    if (anyNA(data) == TRUE) {
        stop("data cannot have missing values")
    }

    # Switch between GAM and spline
    method <- match.arg(method)
    model <- switch(method,
                    gam = gam(y ~ s(x), data = data),
                    spline = lm(y ~ bs(x, df = df), data = data))

    # Obtain fitted values
    fitted_vals <- switch(method,
                          gam = as.vector(predict.gam(model, newdata = data)),
                          spline = as.vector(predict.lm(model, newdata = data)))
    return(fitted_vals)
}
library(ggplot2)
library(dplyr)
library(splines)
library(mgcv)

# Simulate data
wiggly_function <- function(x) {
    stopifnot(is.numeric(x))
    sin(x) - 0.25 * x + 4
}

set.seed(456)
x <- seq(0, 8, length.out = 100)
y <- wiggly_function(x) + rnorm(100, 0, sd = 1)
data <- cbind(x, y) %>% as.data.frame()

# Plot true regression function
wiggly_plot <- ggplot(
    data = data,
    aes(x = x)
) +
    geom_point(
        aes(y = y),
        color = "gray50",
        size = 1.2
) +
    stat_function(
        fun = wiggly_function,
        color = "#000000",
        linewidth = 1.2
) +
    labs(
        title = "Plot of True Regression Function"
)

wiggly_plot

source("./functions/kernel_smoother.R")

# Fit kernel regression
uniform <- kernel_smoother(x = data$x, y = data$y, "uniform", 3)
gaussian <- kernel_smoother(x = data$x, y = data$y, "gaussian", 3)

smoothers <- cbind(data, uniform, gaussian)

colors <- c("Truth" = "#000000", "Uniform" = "#0F5499", "Gaussian" = "#990F3D")

# Plot model fits
smoother_plot <- ggplot(
    data = smoothers,
    aes(x = x)
) +
    geom_point(
        aes(y = y),
        color = "gray50",
        size = 1.2
) +
    geom_line(
        aes(y = uniform,
            color = "Uniform"),
        linewidth = 1.2
) +
    geom_line(
        aes(y = gaussian,
            color = "Gaussian"),
        linewidth = 1.2
) +
    stat_function(
        aes(color = "Truth"),
        fun = wiggly_function,
        linewidth = 1.2
) +
    labs(
        title = "Fitted Kernel Regression Estimates",
        color = "Function"
) +
    scale_color_manual(
        values = colors
)

smoother_plot

# Cross validation to choose optimal bandwidth
source("./functions/mean_sq_err.R")
source("./functions/cross_val.R")

cross_val(x = data$x, y = data$y, "uniform", n_iter = 20)
cross_val(x = data$x, y = data$y, "gaussian", n_iter = 20)

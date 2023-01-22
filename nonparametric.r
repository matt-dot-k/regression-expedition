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
    mapping = aes(
        x = x)
) +
    geom_point(
        aes(y = y),
        color = "gray50",
        size = 1.2
) +
    stat_function(
        fun = wiggly_function,
        color = "#000000",
        size = 1.2
) +
    labs(
        x = "x",
        y = "y",
        title = "Plot of True Regression Function"
)

wiggly_plot


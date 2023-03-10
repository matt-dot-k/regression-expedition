library(ggplot2)
library(dplyr)
library(tidyr)
library(glmnet)

# Define and set theme for plotting
plot_theme <- theme(
    plot.title = element_text(
       size = 16, face = "bold", hjust = 0.5,
       margin = margin(b = 0.25, unit = "cm")),
    plot.subtitle = element_text(
       size = 12),
    axis.title.x = element_text(
       size = 12, face = "bold", angle = 0,
       margin = margin(r = 0.25, unit = "cm")),
    axis.title.y = element_text(
       size = 12, face = "bold", angle = 90,
       margin = margin(r = 0.25, unit = "cm")),
    axis.text = element_text(
       size = 12),
     axis.ticks.length = unit(0.25, "cm"),
    panel.grid.minor.x = element_line(
       linewidth = 0.1, color = "gray50"),
    panel.grid.major.x = element_line(
       linewidth = 0.1, color = "gray50"),
    panel.grid.minor.y = element_line(
       linewidth = 0.1, color = "gray50"),
    panel.grid.major.y = element_line(
       linewidth = 0.1, color = "gray50"),
    panel.background = element_rect(
       fill = "#FFFFFF"),
    panel.border = element_rect(
       color = "#000000", fill = NA, linewidth = 2),
    plot.background = element_rect(
       fill = "#FFFFFF"),
    legend.background = element_rect(
       fill = "#FFFFFF"),
    legend.key = element_rect(
       fill = "#FFFFFF"),
    legend.title = element_text(
       size = 12, face = "bold"),
    legend.text = element_text(
       size = 12)
)

theme_set(plot_theme)

# Read in data and create separate frames for preliminary plotting
firepower <- read.csv("./data/firepower.csv", sep = ",", header = TRUE) %>%
    select(-X, -country, -labor_force, -foreign_exchange_gold,
           -external_debt, -oil_consumption, -oil_proven_reserves) %>%
    mutate(power_index = 1 / power_index) %>%
    as_tibble()

air_power <- firepower %>%
    select(power_index, attack_helicopters, helicopters,
           fighters_interceptors, special_mission) %>%
    pivot_longer(
       cols = c(attack_helicopters, helicopters,
              fighters_interceptors, special_mission),
       names_to = "air_asset",
       values_to = "count")

naval_power <- firepower %>%
    select(power_index, corvettes, destroyers,
           frigates, submarines) %>%
    pivot_longer(
       cols = c(corvettes, destroyers, frigates, submarines),
       names_to = "naval_asset",
       values_to = "count")

air_power_plot <- ggplot(
    data = air_power,
    mapping = aes(
        x = count, y = power_index)
) +
    geom_point(
        size = 1.2,
        color = "#F60552"
) +
    labs(
        x = "Count",
        y = "Power Index",
        title = "Military Power Index by Air Asset Inventory"
) +
    facet_wrap(~ air_asset)

air_power_plot

naval_power_plot <- ggplot(
   data = naval_power,
   mapping = aes(
      x = count, y = power_index)
) +
   geom_point(
      size = 1.2,
      color = "#0F5499"
) +
   labs(
      x = "Count",
      y = "Power Index",
      title = "Military Power Index by Naval Asset Inventory"
) +
   facet_wrap(~ naval_asset)

naval_power_plot

# Fit Ordinary Least Squares
ols_mod <- lm(formula = power_index ~ ., data = firepower)

X <- data.matrix(firepower[, -1])
y <- data.matrix(firepower$power_index)

ridge_mod <- cv.glmnet(x = X, y = y, family = "gaussian", alpha = 0, nfold = 10)
lasso_mod <- cv.glmnet(x = X, y = y, family = "gaussian", alpha = 1, nfold = 10)

par(mfrow = c(1, 2))
plot(ridge_mod$glmnet.fit, xvar = "lambda", lwd = 1.5, main = "Ridge")
plot(lasso_mod$glmnet.fit, xvar = "lambda", lwd = 1.5, main = "Lasso")

ols_coefs <- ols_mod$coefficients

ridge_coefs <- coef(ridge_mod, s = "lambda.min") %>%
   as.matrix() %>%
   as.data.frame() %>%
   rename(Ridge = s1)

lasso_coefs <- coef(lasso_mod, s = "lambda.min") %>%
   as.matrix() %>%
   as.data.frame() %>%
   rename(Lasso = s1)

coef_table <- cbind(ols_coefs, ridge_coefs, lasso_coefs)

print(ridge_mod)
print(lasso_mod)
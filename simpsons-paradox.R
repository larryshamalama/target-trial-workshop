# Load necessary packages
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

ATE <- -1

continuous_treatment <- rnorm(200, mean = 3, sd = 2)
prob_group <- 1 / (1 + exp(-(-6 + 2 * X)))
group <- rbinom(n = 200, size = 1, prob = prob_group)
Y <- 1 + 8 * group + ATE * X + rnorm(200, 0, 1)

data <- data.frame(
  trt = continuous_treatment,
  y = Y,
  Group = ifelse(group, "1", "2")
)


# Regression line by group
regression_lines <- data %>%
  group_by(Group) %>%
  do(model = lm(y ~ trt, data = .)) %>%
  mutate(slope = coef(model)[["trt"]],
         intercept = coef(model)[["(Intercept)"]])

# Marginal regression line (ignoring confounding)
combined_regression_line <- lm(y ~ trt, data = data)
combined_slope <- coef(combined_regression_line)[["trt"]]
combined_intercept <- coef(combined_regression_line)[["(Intercept)"]]

# Plot data
ggplot(data, aes(x = trt, y = y, color = Group)) +
  geom_point() +
  geom_abline(data = regression_lines, aes(slope = slope, intercept = intercept), color = c("#f9766e", "#00bfc4"), linewidth = 0.5) +
  geom_abline(intercept = combined_intercept, slope = combined_slope, color = "black", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Illustration of Simpson's Paradox", trt = "Treatment", y = "Outcome")

# Check if the required packages are installed and install them if they are not

if (!requireNamespace("tidyverse")) {
  install.packages("tidyverse")
}

if (!requireNamespace("ggplot2")) {
  install.packages("ggplot2")
}

# data available here
if (!requireNamespace("designmatch")) {
  install.packages("designmatch")
}

library(tidyverse)
library(ggplot2)
library(designmatch)

# Load the data

data(lalonde) # load the data
attach(lalonde)

head(data)

# Exploratory data analysis

table(lalonde$treatment)

long_data <- lalonde %>%
  pivot_longer(cols = c(re74, re75, re78), names_to = "variable", values_to = "value")

ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(title = "Boxplots of 1974, 1975 and 1978 earnings",
       x = "Variable",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

density_re74 <- density(lalonde$re74)
density_re75 <- density(lalonde$re75)
density_re78 <- density(lalonde$re78)

plot(density_re74, main = "Kernel Density of 1974, 1975 and 1978 earnings", 
     xlab = "Earnings", ylab = "Density", xlim = range(c(density_re74$x, density_re75$x, density_re78$x)), 
     ylim = range(c(density_re74$y, density_re75$y, density_re78$y)), col = "red", lwd = 2)

lines(density_re75, col = "green", lwd = 2)
lines(density_re78, col = "blue", lwd = 2)

legend("topright", legend = c("re74", "re75", "re78"), col = c("red", "green", "blue"), lwd = 2)

# Analysis

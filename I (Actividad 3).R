# Tasas de crecimiento PIBPC
library(dplyr)
library(purrr)
library(ggplot2)

g <- c(0.001, 0.005, 0.03, 0.05,0.1)
t <- seq(0, 25, length.out = 100)

# Create the tibble for each g
cont <- map_dfr(g, ~ tibble(g_value = .x, t = t, value = 100*exp(t * .x)))
dis <- map_dfr(g, ~ tibble(g_value = .x, t = t, value = 100*(1 +.x)^t))

ggplot(cont, aes(t, value, color = factor(g_value))) +
  geom_line(size = 1) + 
  labs(title = "Continuous",
       subtitle = expression(100 * exp(g * t)),
       color = "Rates") +
  scale_y_continuous(limits = c(100, 200)) + # Set y-axis limits
  theme_classic()
       

ggplot(dis, aes(t, value, color = factor(g_value))) +
  geom_line(size = 1) +
  labs(title = "Discrete",
       subtitle = expression(100(1+x)^t),
       color = "Rates") +
  scale_y_continuous(limits = c(100, 200)) + # Set y-axis limits
  theme_classic()


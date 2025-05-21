rm(list = ls())

library(tidyverse)
library(pwt10)

pwt <- pwt10.01 %>% 
  group_by(country) %>% 
  reframe(
    year,
    country,
    y = rgdpo / pop,              # GDP per capita
    g = c(NA, diff(log(y))),      # Growth rate with proper alignment
    s = csh_i,                    # Rename save
    n = c(NA, diff(log(pop))),    # Population growth
    delta,                        # Depreciation rate
    r = g + delta + n
  )

aggregated <- pwt %>%
  group_by(country) %>%
  summarise(across(-c(1, 2), function(x){mean(x, na.rm = TRUE)}), .groups = "drop")

# Test for all countries
ggplot(aggregated, aes(s,g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F)

cor(aggregated$g, aggregated$s)


# Test for OECD countries
# OECD countries vector
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Republic of Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Turkey", "United Kingdom", "United States of America"
)

# Example: filter the pwt dataset for OECD countries
oecd <- aggregated[aggregated$country %in% oecd_countries, ]


ggplot(oecd, aes(s,g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F)

cor(oecd$g, oecd$s)

# Test for all countries
ggplot(aggregated, aes(r,s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F)

cor(aggregated$r, aggregated$s, use = "na.or.complete")


# Example: filter the pwt dataset for OECD countries
ggplot(oecd, aes(r,s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F)

cor(oecd$r, oecd$s)


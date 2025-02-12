rm(list = ls())

library(pwt10)
library(tidyverse)

# Load Penn World Table
pwt <- pwt10.01 %>% 
  group_by(year) %>% 
  reframe(
    isocode,
    year,
    gdppw = cgdpo/emp,
    kpw = cn/emp,
    labsh
  )

# Check names
names(pwt)

# Filter for Mexico
pwt %>% 
  filter(isocode %in% c("MEX", "USA", "CHL", "KOR"),
         year >= 1980 & year <= 2020) %>% 
  group_by(isocode) %>% 
  arrange(year) %>%  # Arrange in ascending order for lag() to work correctly
  reframe(
    gy = mean(gdppw/lag(gdppw)-1, na.rm = TRUE),  # Growth rate of real GDP
    gk = mean(kpw/lag(kpw)-1, na.rm = TRUE), # Growth rate of capital stock
    alpha = 1 - mean(labsh, na.rm = TRUE),
    RS = gy - alpha * gk,
    Sh_TFP = RS / gy * 100,
    Sh_k = gk / gy * 100
  )

# Ratio GDP per worker vesus USA
pwt %>% 
  filter(isocode %in% c("MEX", "USA", "CHL", "KOR"),
         year >= 1980 & year <= 2020) %>% 
  arrange(year) %>%  # Arrange in ascending order for lag() to work correctly
  group_by(year) %>%
  mutate(
    ratio = gdppw / gdppw[isocode == "USA"])  %>% # Ratio relative to USA
  filter(isocode != "USA") %>% 
  ggplot(aes(year, ratio, col = isocode)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Ratio GDP per worker vesus USA",
    y = "Ratio",
    x = "Year",
    color = "Country"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

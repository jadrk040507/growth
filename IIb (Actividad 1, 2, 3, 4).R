rm(list = ls())

library(pwt10)
library(tidyverse)

pwt <- pwt10.01 %>% 
  filter(year >= 1980 & year <= 2020) %>% 
  mutate(
    gdppw = cgdpo / emp,  # GDP per worker
    kpw = cn / emp        # Capital stock per worker
  ) %>% 
  group_by(isocode) %>% 
  arrange(year) %>% 
  mutate(
    gy = (gdppw/lag(gdppw)-1)*100,  # GDP per worker growth rate
    gk = (kpw/lag(kpw)-1)*100      # Capital per worker growth rate
  )

pwt %>% 
  filter(year >= 1980 & year <= 2023) %>% 
  group_by(isocode) %>% 
  reframe(
    across(c(gy, gk, labsh), ~ mean(.x, na.rm = TRUE)),
    alpha = 1 - labsh,
    RS = gy - alpha * gk,
    Sh_TFP = 100 * RS / gy,
    Sh_k = 100 * alpha * gk / gy
  ) %>%
  filter(isocode %in% c("CHN", "ARG", "USA", "CHL", "KOR", "MEX"))

# Ratio GDP per worker versus USA
pwt %>% 
  filter(isocode %in% c("CHN", "ARG", "MEX", "USA", "CHL", "KOR"),
         year >= 1980 & year <= 2020) %>% 
  arrange(year) %>%  # Arrange in ascending order for lag() to work correctly
  group_by(year) %>%
  mutate(
    ratio = gdppw / gdppw[isocode == "USA"])  %>% # Ratio relative to USA
  filter(isocode != "USA") %>% 
  ggplot(aes(year, ratio, col = isocode)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Ratio GDP per worker versus USA",
    y = "Ratio",
    x = "Year",
    color = "Country"
  ) +
  theme(
  plot.title = element_text(hjust = 0.5),
  legend.position = "bottom"
)

# Ratio HC versus USA
pwt %>% 
  filter(isocode %in% c("CHN", "ARG", "MEX", "USA", "CHL", "KOR"),
         year >= 1980 & year <= 2020) %>% 
  arrange(year) %>%  # Arrange in ascending order for lag() to work correctly
  group_by(year) %>%
  mutate(
    ratio = hc / hc[isocode == "USA"])  %>% # Ratio relative to USA
  filter(isocode != "USA") %>% 
  ggplot(aes(year, ratio, col = isocode)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Ratio HC versus USA",
    y = "Ratio",
    x = "Year",
    color = "Country"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

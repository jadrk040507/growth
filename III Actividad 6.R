rm(list = ls())
library(pwt10)
library(tidyverse)
library(scales)
library(ggthemr)

ggthemr('fresh')

pwt <- pwt10.01

df <- pwt %>%
  group_by(country) %>%
  # filter(country == "Mexico") %>% 
  reframe(
    year,
    y = rgdpo / pop,              # GDP per capita
    g = c(NA, diff(log(y))),      # Growth rate with proper alignment
    k = cn / cgdpo,               # Capital-output ratio
    alpha = 1 - labsh,            # Capital share
    i = irr                       # Investment rate
  )

# 1950-1970
aggregate(. ~ country, 
          data = df[df$year %in% 1950:1970, !names(df) %in% "year"], 
          mean, na.rm = TRUE)

# 1970-1990
aggregate(. ~ country, 
          data = df[df$year %in% 1970:1990, !names(df) %in% "year"], 
          mean, na.rm = TRUE)

# 1990-2010
aggregate(. ~ country, 
          data = df[df$year %in% 1990:2010, !names(df) %in% "year"], 
          mean, na.rm = TRUE)

# Plots
countries <- c("Mexico", "United States of America", "Brazil", "Chile", "Republic of Korea", "Argentina")

ggplot(df[df$country %in% countries,], aes(year, y, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "GDP per capita",
    y = "",
    color = "Country") + 
  scale_y_continuous(labels = scales::label_dollar(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) 



ggplot(df[df$country %in% countries,], aes(year, g, color = country)) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = year, y = 0), linewidth = 1, linetype = 4,  color = "black") +
  labs(
    title = "Growth",
    y = "",
    color = "Country") + 
  scale_y_continuous(labels = scales::label_percent(), breaks = scales::breaks_extended(n = 8)) +  
  theme(plot.title = element_text(hjust = 0.5)) 



ggplot(df[df$country %in% countries,], aes(year, k, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Capital per capita",
    y = "",
    color = "Country") + 
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) 



ggplot(df[df$country %in% countries,], aes(year, i, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Interest rate",
    y = "",
    color = "Country") + 
  scale_y_continuous(labels = scales::label_percent(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) +



  library(ggplot2)

ggplot(df[df$country %in% countries, ], aes(x = year, y = alpha, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Capital Share",
    y = "", 
    color = "Country") + 
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))





# Check for time and spatial trends
summary(lm(g ~ year + country, df[df$country %in% countries, ]))
summary(lm(k ~ year + country, df[df$country %in% countries, ]))
summary(lm(i ~ year + country, df[df$country %in% countries, ]))
summary(lm(alpha ~ year + country, df[df$country %in% countries, ]))


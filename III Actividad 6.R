# Kaldor

rm(list = ls())
library(pwt10)
library(tidyverse)
library(scales)
library(ggthemr)

ggthemr('fresh')

pwt <- pwt10.01

df <- pwt %>%
  group_by(country) %>%
  reframe(
    year,
    y = rgdpo / pop,              # GDP per capita
    g = c(NA, diff(log(y))),      # Growth rate with proper alignment
    k = cn / cgdpo,               # Capital-output ratio
    alpha = 1 - labsh,            # Capital share
    i = irr,                      # Investment rate
    delta,                        # Depreciation rate
    n = c(NA, diff(log(pop))),    # Population growth
    ly = log(y),                  # Log of GRP per capita
    s = csh_i,                    # Rename save
    hc                            # Human capital
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
  scale_y_log10(labels = scales::label_dollar(), breaks = scales::breaks_extended(n = 8)) +
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
  theme(plot.title = element_text(hjust = 0.5))



ggplot(df[df$country %in% countries, ], aes(year, alpha, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Capital Share",
    y = "", 
    color = "Country") + 
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(df[df$country %in% countries, ], aes(year, n, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Population Growth",
    y = "", 
    color = "Country") + 
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(df[df$country %in% countries, ], aes(year, delta, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Delta",
    y = "", 
    color = "Country") + 
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

# Check for time and spatial trends
feols(g ~ year | country, data = df[df$country %in% countries, ], cluster = ~ country)
feols(k ~ year | country, data = df[df$country %in% countries, ], cluster = ~ country)
feols(i ~ year | country, data = df[df$country %in% countries, ], cluster = ~ country)
feols(alpha ~ year | country, data = df[df$country %in% countries, ], cluster = ~ country)
feols(delta ~ year | country, data = df[df$country %in% countries, ], cluster = ~ country)
feols(n ~ year | country, data = df[df$country %in% countries, ], cluster = ~ country)


# Convergence regression
# g_{j, t} = A + B ln(y_{j, t-l}) + e_{j,t}

conv_df <- df %>%
  group_by(country) %>%
  filter(year >= 1970 & year <= 2020) %>% 
  reframe(
    gy = mean(g, na.rm = TRUE),    # Mean growth rate
    ly = first(ly),                # First `ly` value
    s = last(s),                   # Last `s` value
    n = last(n),                   # Last `n` value
    delta = last(delta),           # Last `delta` value
    hc = last(hc)                  # Last `hc` value
  )

# Convergencia no condicional
summary(lm(gy ~ ly, conv_df))

# Convergencia condicional
summary(lm(gy ~ . -n -delta -country, conv_df))
summary(lm(gy ~ . -country, conv_df))


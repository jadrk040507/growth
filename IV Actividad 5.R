# Test for convergence

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


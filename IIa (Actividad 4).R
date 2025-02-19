rm(list = ls())

library(tidyverse)
library(pwt10)

pwt <- pwt10.01

usa <- pwt %>% 
  filter(country == "United States of America") %>% 
  mutate(year_1950 = year-1950)

usa %>% 
  reframe(mean = mean(labsh))

usa %>% 
  ggplot(aes(year, labsh)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  labs(
    title = "Labor share of GDP",
    x = "Year",
    y = "% of GDP"
       ) +
  theme_bw()

model <- lm(labsh ~ year_1950, data = usa)
summary(model)


rm(list = ls())

library(pwt10)
library(WDI)
library(dplyr)

pwt <- pwt10.0

wdi <- WDI(
  country = "all",
  indicator = c("NY.GDP.MKTP.PP.KD", "NY.GDP.PCAP.PP.KD"),
  start = 1960,
  end = NULL,
) %>% 
  na.omit()

growth_gdppc <- pwt %>% 
  reframe(country, year, rgdppc = rgdpo/pop) %>% 
  group_by(country) %>% 
  mutate(g = (rgdppc/lag(rgdppc) - 1)*100)

growth_gdppc %>% 
  filter(country == "Mexico")

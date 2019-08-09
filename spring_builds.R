library(data.table)
library(tidyverse)

a <- fread("data/grt_report_export_79_20190808080917.csv")
b <- fread("data/grt_report_export_79_20190808102453.csv")
c <- fread("data/grt_report_export_79_20190808104319.csv")

build.df <- rbind(a,b,c)

# get location

build.df$territory <- substr(build.df$`Region Name`, 1, 4)

levels(as.factor(build.df$territory))
table(build.df$territory)

build.df <- build.df %>% 
  select(`Customer Code`, `Region Name`, `Qty Assist`, territory)

build.present <- build.df %>% 
  group_by(territory, `Customer Code`) %>% 
  summarise(qty.assist = sum(`Qty Assist`)) %>% 
  mutate(build = cut(qty.assist, breaks=c(-Inf, 0, Inf), labels=c(0,1))) %>% 
  select(-c(qty.assist, `Customer Code`)) %>% 
  group_by(territory, build) %>%
  summarise (n = n()) %>%
  mutate(freq = round((n / sum(n)) * 100, 2))


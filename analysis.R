library(tidyverse)
library(dplyr)
library(xts)
library(strucchange)

setwd("D:/Roshan/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/Data")

# Data
public_finance <- read.csv("data.csv")
debt <- read.csv("debt.csv")

# Merge data
df <- public_finance %>%
  rename(year = FY1) %>%
  left_join(debt, by = "year") 

str(df)

# Data Structure
df$year = as.integer(df$year)

df <- df%>%
  mutate(across(c(Recurrent, Capital, Interest, Revenue, GDP), ~ as.numeric(gsub(",", "", .x))))

# Required Variables
df1 <- df%>%
  mutate(
    pb = Revenue - Recurrent - Capital + Interest,
    pb_gdp = pb/GDP,
    debt_gdp = lag(debt)/GDP
  ) %>%
  filter(year >= 2003)

# Plot
plot1 <- ggplot(df1, aes(year)) +
  # geom_line(aes(y = pb_gdp, color = "black")) +
  geom_line(aes(y = debt_gdp, color = "red"))

plot1

# Structural Breaks Bai - Perron
break_yr <- breakpoints(pb_gdp ~ debt_gdp, data = df1, h = 0.20)   # or h = 0.25
summary(break_yr)
plot(break_yr)

break_forced <- breakpoints(break_yr, breaks = 1)
breakdates(break_forced)
df1$year[break_forced$breakpoints]
confint(break_yr)

# Chow test
break_index <- which(df1$year == 2016)
chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
print(chow_test)

sctest(Fstats(pb_gdp ~ debt_gdp, data = df1, from = 0.15, to = 0.85), type = "supF")

t <- Fstats(pb_gdp ~ debt_gdp, data = df1, from = 0.15, to = 0.85)
plot(t)
# break_index <- which(df1$year == 2017)
# chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
# print(chow_test)

# break_index <- which(df1$year == 2018)
# chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
# print(chow_test)

















# Time series object
# data_cols <- df1[, c("pb_gdp", "debt_gdp")]
# finance_ts <- ts(
#   data = data_cols,
#   start = df$year,
#   frequency = 1
# )














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

# Time series object
data_cols <- df1[, c("pb_gdp", "debt_gdp")]
finance_ts <- ts(
  data = data_cols,
  start = df$year,
  frequency = 1
)

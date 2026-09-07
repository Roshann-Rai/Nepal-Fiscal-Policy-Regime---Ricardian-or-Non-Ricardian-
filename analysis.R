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
break_yr <- breakpoints(pb_gdp ~ debt_gdp, data = df1, h = 0.15)
summary(break_yr)
plot(break_yr)

break_forced <- breakpoints(break_yr, breaks = 1)
df1$year[break_forced$breakpoints]
confint(break_forced, breaks = 1)

# Chow test
break_index <- which(df1$year == 2016)
chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
print(chow_test)

# years_to_test <- c(2015, 2016, 2017)
# results <- sapply(years_to_test, function(yr) {
#   mydata$brk <- as.numeric(mydata$year >= yr)
#   full    <- lm(y ~ x1 + x2 * brk, data = mydata)
#   reduced <- lm(y ~ x1 + x2, data = mydata)
#   a <- anova(reduced, full)
#   c(F = a$F[2], p = a$`Pr(>F)`[2])
# })
# colnames(results) <- years_to_test
# results
















# Time series object
# data_cols <- df1[, c("pb_gdp", "debt_gdp")]
# finance_ts <- ts(
#   data = data_cols,
#   start = df$year,
#   frequency = 1
# )














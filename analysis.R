library(tidyverse)
library(dplyr)
library(xts)
library(strucchange)

# setwd("https://github.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/Data")

# Data
public_finance <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/data.csv")
debt <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/debt.csv")

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
## h = 0.2 means 20% of sample are included in 1 segment
break_yr <- breakpoints(debt_gdp ~ pb_gdp, data = df1, h = 0.20)   # or h = 0.25
summary(break_yr)
plot(break_yr)

break_forced <- breakpoints(break_yr, breaks = 1)  # Gives 1 break 
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

# Dummy Variable
df1 <- df1 %>%
  arrange(year) %>%
  mutate(break_yr = ifelse(year > 2015, 1, 0))

# Check the data
table(df1$year, df1$break_yr)

# Required Variables
df2 <- df1 %>%
  mutate(
    # Lag variables
    pb_lag = lag(pb_gdp, 1),
    debt_lag = lag(debt_gdp, 1),
    # Interaction term
    pb_dummy = pb_lag*break_yr,
    debt_dummy = debt_lag*break_yr
  )

# Lag length Selection
# Because of fewer observations we select lag = 1.

# Model
pb <- lm(pb_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, data = df2)
debt <- lm(debt_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, data = df2)
summary(pb)
summary(debt)
pb_nobreak <- lm(pb_gdp ~ pb_lag + debt_lag, data = df2)

# summary(pb_nobreak)

# anova(pb, pb_nobreak)

# anova(pb_nobreak, pb)
# break_index <- which(df1$year == 2017)
# chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
# print(chow_test)

# break_index <- which(df1$year == 2018)
# chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
# print(chow_test)

# Coefficients
get_regime_coefs <- function(fit, regime = c("pre","post")) {
  regime <- match.arg(regime)
  b <- coef(fit)
  const    <- b["(Intercept)"]
  pb_l1    <- b["pb_lag"]
  debt_l1  <- b["debt_lag"]
  if (regime == "post") {
    const   <- const   + b["break_yr"]
    pb_l1   <- pb_l1   + b["pb_dummy"]
    debt_l1 <- debt_l1 + b["debt_dummy"]
  }
  c(const = const, pb_lag = pb_l1, debt_lag = debt_l1)
}

# Balance equation, both regimes
coefs_s_pre  <- get_regime_coefs(fit_s, "pre")
coefs_s_post <- get_regime_coefs(fit_s, "post")

# Debt equation, both regimes
coefs_w_pre  <- get_regime_coefs(fit_w, "pre")
coefs_w_post <- get_regime_coefs(fit_w, "post")















# Time series object
# data_cols <- df1[, c("pb_gdp", "debt_gdp")]
# finance_ts <- ts(
#   data = data_cols,
#   start = df$year,
#   frequency = 1
# )














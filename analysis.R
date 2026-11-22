library(tidyverse)
library(dplyr)
library(xts)
library(strucchange)
library(vars)
# setwd("https://github.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/Data")

# Data
public_finance <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/data.csv")
debt <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/debt.csv")
data <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/pb.csv")
gdp <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/gdp.csv")
revenue <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/revenue.csv")

# Merge data
df <- public_finance %>%
  rename(year = FY1) %>%
  left_join(debt, by = "year") %>%
  left_join(revenue, by = "year") %>%
  dplyr::select(-GDP) %>%
  left_join(gdp, by = "year")

# Data Structure
df$year = as.integer(df$year)

df <- df%>%
  mutate(across(c(Recurrent, Capital, Interest, Revenue, GDP), ~ as.numeric(gsub(",", "", .x)))) %>%
  dplyr::select(-Revenue)

# Required Variables
df1 <- df%>%
  dplyr::select(-FY) %>%
  mutate(
    pb = revenue - Recurrent - Capital + Interest
    # pb_gdp = pb/GDP,
    # debt_gdp = lag(debt)/GDP # debt beginning of the year
  )

view(df1)

df2 <- data %>%
  rename(year = FY1) %>%
  mutate(pb = -(PB_1 - grant)) %>%
  dplyr::select(year, pb)

view(df2)

df3 <- df1 %>%
  left_join(df2 %>%
              select(year, df2_pb = pb), by = "year") %>%
  mutate(pb = coalesce(pb, df2_pb)) %>%
  dplyr::select(year, debt, GDP, pb)

view(df3)

# 1996 data
df_1996 <- data.frame(
  year = 1996,
  debt = 160102.2,
  GDP = 248913.0,
  pb = NA
)

# Merge with df3
merged_df <- bind_rows(df_1996, df3)
str(merged_df)

# Final Dataset
merged_df2 <- merged_df %>%
  arrange(year) %>%
  mutate(pb_gdp = pb/GDP, debt_gdp = lag(debt)/GDP) %>%
  filter(year >= 1997) 

view(merged_df2)

# # Plot
# plot1 <- ggplot(merged_df2, aes(year)) +
#   # geom_line(aes(y = pb_gdp, color = "black")) +
#   geom_line(aes(y = debt_gdp, color = "red"))
# 
# plot1

# Structural Breaks Bai - Perron
## h = 0.2 means 20% of sample are included in 1 segment
break_yr <- breakpoints(pb_gdp ~ debt_gdp, data = merged_df2, h = 0.20)   # or h = 0.25
summary(break_yr)
plot(break_yr)

break_forced <- breakpoints(break_yr, breaks = 1)  # Gives 1 break 
breakdates(break_forced)
merged_df2$year[break_forced$breakpoints]
confint(break_yr)

# Chow test
break_index <- which(merged_df2$year == 2016)
chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = merged_df2, point = break_index)
print(chow_test)

sctest(Fstats(pb_gdp ~ debt_gdp, data = merged_df2, from = 0.15, to = 0.85), type = "supF")

t <- Fstats(pb_gdp ~ debt_gdp, data = merged_df2, from = 0.15, to = 0.85) 
plot(t)

# Dummy Variable
merged_df2 <- merged_df2 %>%
  arrange(year) %>%
  mutate(break_yr = ifelse(year > 2015, 1, 0))

# Check the data
table(merged_df2$year, merged_df2$break_yr)

# Required Variables
merged_df3 <- merged_df2 %>%
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
# Baseline model without structural breaks
pb <- lm(pb_gdp ~ pb_lag + debt_lag, data = merged_df3)
debt <- lm(debt_gdp ~ pb_lag + debt_lag, data = merged_df3)
summary(pb)
summary(debt)

df_var <- VAR(
  merged_df3 %>% dplyr::select(pb_gdp, debt_gdp),
  p = 1,
  type = "const"
)
  
summary(df_var)

# Impulse response:
# Response of liabilities (w) to a shock in primary surplus (s)

irf_1 <- irf(
  df_var,
  impulse = "pb_gdp",
  response = "debt_gdp",
  n.ahead = 15,
  boot = TRUE,
  runs = 1000,
  ci = 0.95
)


# Plot
h   <- 0:15
est <- irf_1$irf$pb_gdp[, "pb_gdp"]
lo  <- irf_1$Lower$pb_gdp[, "pb_gdp"]
up  <- irf_1$Upper$pb_gdp[, "pb_gdp"]

plot(h, est, type = "l", lwd = 2,
     xlim = c(0, 15), ylim = range(lo, up),
     xaxs = "i",                      # no padding on the x-axis
     xlab = "Horizon", ylab = "pb_gdp",
     main = "Orthogonal IRF: pb_gdp → pb_gdp\n95% bootstrap CI, 1000 runs")
lines(h, lo, col = "red", lty = 2)
lines(h, up, col = "red", lty = 2)
abline(h = 0, col = "red")


# Model with structural breaks
pb_brk <- lm(pb_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, data = merged_df3)
debt_brk <- lm(debt_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, data = merged_df3)
summary(pb_brk)
summary(debt_brk)

# Testing if the introduction of structural break improves the explanatory power of model
anova(pb, pb_brk)
anova(debt, debt_brk)
# p-value < 0.05 so yes.

# Stability Test
# CUSUM Test
# Stability test without structural breaks
cusum_pb <- efp(pb_gdp ~ pb_lag + debt_lag, 
                data = merged_df3,
                type = "OLS-CUSUM")
cusum_debt <- efp(debt_gdp ~ pb_lag + debt_lag, 
                  data = merged_df3,
                  type = "OLS-CUSUM")

plot(cusum_pb)
plot(cusum_debt)

sctest(cusum_pb)
sctest(cusum_debt)

# Stability test with structural breaks
cusum_pb_brk <- efp(pb_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, 
                data = merged_df3,
                type = "OLS-CUSUM")
cusum_debt_brk <- efp(debt_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, 
                  data = merged_df3,
                  type = "OLS-CUSUM")

plot(cusum_pb_brk)
plot(cusum_debt_brk)

sctest(cusum_pb_brk)
sctest(cusum_debt_brk)

# p > 0.05 for both, so failed to reject H0: Regression coefficients are stable over time.

# VAR eigen value test (dynamic stability)
# Baseline model
# Construct coefficient matrix
base_model <- rbind(
  c(coef(pb)["pb_lag"], coef(pb)["debt_lag"]),
  c(coef(debt)["pb_lag"], coef(debt)["debt_lag"])
)

# Eigenvalues
roots_base <- eigen(base_model)$values

# Modulus
Mod(roots_base)

# Stability condition
all(Mod(roots_base) < 1)

# Structural Break Model Eigen value test
# Pre 2016
pre_2016 <- rbind(
  c(coef(pb_brk)["pb_lag"], coef(pb_brk)["debt_lag"]),
  c(coef(debt_brk)["pb_lag"], coef(debt_brk)["debt_lag"])
)

roots_pre <- eigen(pre_2016)$values

Mod(roots_pre)

all(Mod(roots_pre)<1)

# Post 2018
A_post <- rbind(
  c(
    coef(pb_brk)["pb_lag"] + coef(pb_brk)["pb_dummy"],
    coef(pb_brk)["debt_lag"] + coef(pb_brk)["debt_dummy"]
  ),
  c(
    coef(debt_brk)["pb_lag"] + coef(debt_brk)["pb_dummy"],
    coef(debt_brk)["debt_lag"] + coef(debt_brk)["debt_dummy"]
  )
)

roots_post <- eigen(A_post)$values

Mod(roots_post)
all(Mod(roots_post) < 1)


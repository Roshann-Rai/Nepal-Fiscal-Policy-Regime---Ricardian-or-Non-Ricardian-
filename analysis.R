library(tidyverse)
library(dplyr)
library(xts)
library(strucchange)

# setwd("https://github.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/Data")

# Data
public_finance <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/data.csv")
debt <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/debt.csv")
data <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/pb.csv")
gdp <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Nepal-Fiscal-Policy-Regime---Ricardian-or-Non-Ricardian-/refs/heads/main/Data/gdp.csv")

# Merge data
df <- public_finance %>%
  rename(year = FY1) %>%
  left_join(debt, by = "year") 

df

# Data Structure
df$year = as.integer(df$year)

df <- df%>%
  mutate(across(c(Recurrent, Capital, Interest, Revenue, GDP), ~ as.numeric(gsub(",", "", .x))))
df

# Required Variables
df1 <- df%>%
  mutate(
    pb = Revenue - Recurrent - Capital + Interest,
    pb_gdp = pb/GDP,
    debt_gdp = lag(debt)/GDP
  ) %>%
  filter(year >= 2003) %>%
  select(year, pb)

view(df1)

df2 <- data %>%
  rename(year = FY1) %>%
  mutate(pb = -(PB_1 - grant)) %>%
  select(year, pb)

view(df2)

# Merged data
df_1996 <- data.frame(
  year = 1996,
  pb = NA,
  debt = 160102.2,
  GDP = 248913.0
)
merged_df <- bind_rows(df2, df1) 

merged_df1 <- merged_df %>%
  left_join(debt, by = "year") %>%
  left_join(gdp, by = "year")

view(merged_df1)

merged_df_1996 <- bind_rows(df_1996, merged_df1)
view(merged_df_1996)

# Required data
merged_df_1996$year <- as.integer(merged_df_1996$year)

merged_df2 <- merged_df_1996 %>%
  mutate(pb_gdp = pb/GDP, debt_gdp = lag(debt)/GDP) %>%
  filter(year >= 1997) 

view(merged_df2)

# Plot
plot1 <- ggplot(merged_df2, aes(year)) +
  # geom_line(aes(y = pb_gdp, color = "black")) +
  geom_line(aes(y = debt_gdp, color = "red"))

plot1

# Structural Breaks Bai - Perron
## h = 0.2 means 20% of sample are included in 1 segment
break_yr <- breakpoints(debt_gdp ~ pb_gdp, data = merged_df2, h = 0.20)   # or h = 0.25
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

sctest(Fstats(pb_gdp ~ debt_gdp, data = df1, from = 0.15, to = 0.85), type = "supF")

t <- Fstats(pb_gdp ~ debt_gdp, data = df1, from = 0.15, to = 0.85) 
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
pb <- lm(pb_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, data = merged_df3)
debt <- lm(debt_gdp ~ break_yr + pb_lag + debt_lag + pb_dummy + debt_dummy, data = merged_df3)
summary(pb)
summary(debt)

# pb_nobreak <- lm(pb_gdp ~ pb_lag + debt_lag, data = df2)

# summary(pb_nobreak)

# anova(pb, pb_nobreak)

# anova(pb_nobreak, pb)
# break_index <- which(df1$year == 2017)
# chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
# print(chow_test)

# break_index <- which(df1$year == 2018)
# chow_test <- sctest(pb_gdp ~ debt_gdp, type = "Chow", data = df1, point = break_index)
# print(chow_test)

# Coefficnents
coefs <- function(fit) {
  b <- coef(fit)
  data.frame(
    period = c("Pre_2016", "Post_2016"),
    Intercept = c(
      b["(Intercept)"],
      b["(Intercept)"]+b["break_yr"]
    ),
    pb = c(
      b["pb_lag"],
      b["pb_lag"]+b["pb_dummy"]
    ),
    debt = c(
      b["debt_lag"],
      b["debt_lag"]+b["debt_dummy"]
    )
  )
}

pb_coefs <- coefs(pb)
debt_coefs <- coefs(debt)

pb_coefs
debt_coefs


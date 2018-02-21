# Set working directory
setwd("/Users/mariomoreno/Desktop/Grad School/Stat II/Memo")

#load libraries
install.packages("lmtest")
install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(lmtest)

#output file
sink(file = "Afghanistan output.txt")

#import data
afg_data <- read.csv("Afghanistan.csv")

# This regression estimates the relationship between military aid and direct attacks
dir <- lm(direct_attack ~ CERP_all + (CERP_all*govt_control), data = afg_data)

# This regression estimates the relationship between military aid and indirect attacks
indir <- lm(indirect_attack ~ CERP_all + (CERP_all*govt_control), data = afg_data)

# This regression estimates the relationship between military aid, direct attacks
# government control of districts
dir_control <- lm(direct_attack ~ CERP_all + govt_control + (CERP_all*govt_control), data = afg_data)

# This regression estimates the relationship between military aid, indirect attacks
# and government control of districts
indir_control <- lm(indirect_attack ~ CERP_all + govt_control + (CERP_all*govt_control), data = afg_data)

# The following two regressions estimate:
# - relationship between small aid spending with direct attacks and government control
# - relationship between small aid spending with indirect attacks and government control 
small_aid_direct <- lm(direct_attack ~ CERP_small + govt_control + (CERP_small*govt_control), data = afg_data)
small_aid_indirect <- lm(indirect_attack ~ CERP_small + govt_control + (CERP_small*govt_control), data = afg_data)

# The following two regressions estiamte:
# - relationship between large aid spending with direct attacks and government control
# - relationship between large aid spending with indirect attacks and government control
large_aid_direct <- lm(direct_attack ~ CERP_large + govt_control + (CERP_large*govt_control), data = afg_data)
large_aid_indirect <- lm(indirect_attack ~ CERP_large + govt_control + (CERP_large*govt_control), data = afg_data)

# Builds output tables
stargazer(small_aid_direct, small_aid_indirect, large_aid_direct, large_aid_indirect, type="text")

# Plotting number of indirect attacks v small aid allocation
ggplot (data=afg_data, mapping = aes(x = indirect_attack, y = CERP_small)) + geom_point() +
  geom_smooth (method = "lm", se = FALSE) +
  labs (x = "number of indirect attacks", y = "small aid allocation (per capita)")

# Plotting number of direct attacks v small aid allocation
ggplot (data=afg_data, mapping = aes(x = direct_attack, y = CERP_small)) + geom_point() +
  geom_smooth (method = "lm", se = FALSE) +
  labs (x = "number of direct attacks", y = "small aid allocation (per capita)")

# Plotting number of direct attacks v large aid allocation
ggplot (data=afg_data, mapping = aes(x = indirect_attack, y = CERP_large)) + geom_point() +
  geom_smooth (method = "lm", se = FALSE) +
  labs (x = "number of indirect attacks", y = "large aid allocation (per capita)")

# Plotting number of direct attacks v large aid allocation
ggplot (data=afg_data, mapping = aes(x = direct_attack, y = CERP_large)) + geom_point() +
  geom_smooth (method = "lm", se = FALSE) +
  labs (x = "number of direct attacks", y = "large aid allocation (per capita)")

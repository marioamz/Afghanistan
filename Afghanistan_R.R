# Set working directory
setwd("/Users/mariomoreno/Desktop/Grad School/Stat II/Policy Memo")

#load libraries
install.packages("lmtest")
library(tidyverse)
library(lmtest)

#output file
#sink(file = "Afghanistan output.txt")

#import data
afg_data <- read.csv("Afghanistan.csv")

# This regression estimates the relationship between military aid and direct attacks
dir <- lm(direct_attack ~ CERP_all, data = afg_data)
summary(dir)

# This regression estimates the relationship between military aid and indirect attacks
indir <- lm(indirect_attack ~ CERP_all, data = afg_data)
summary(indir)

# This regression estimates the relationship between military aid, direct attacks
# government control of districts
dir_control <- lm(direct_attack ~ CERP_all + govt_control, data = afg_data)
summary(dir_control)

# This regression estimates the relationship between military aid, indirect attacks
# and government control of districts
indir_control <- lm(indirect_attack ~ CERP_all + govt_control, data = afg_data)
summary(indir_control)

# The following two regressions estimate:
  # - relationship between small aid spending with direct attacks and government control
  # - relationship between small aid spending with indirect attacks and government control 
small_aid_direct <- lm(direct_attack ~ CERP_small + govt_control, data = afg_data)
small_aid_indirect <- lm(direct_attack ~ CERP_small + govt_control, data = afg_data)
summary(small_aid_direct)
summary(small_aid_indirect)

# The following two regressions estiamte:
  # - relationship between large aid spending with direct attacks and government control
  # - relationship between large aid spending with indirect attacks and government control
large_aid_direct <- lm(direct_attack ~ CERP_large + govt_control, data = afg_data)
large_aid_indirect <- lm(indirect_attack ~ CERP_large + govt_control, data = afg_data)
summary(large_aid_direct)
summary(large_aid_indirect)

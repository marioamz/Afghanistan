# Set working directory
setwd("/Users/mariomoreno/Desktop/Grad School/Stat II/Memo")

#load libraries
install.packages("lmtest")
install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(lmtest)

#output file
# sink(file = "Afghanistan output.txt")

#import data
afg_data <- read.csv("Afghanistan.csv")

# This regression estimates the relationship between military aid and direct attacks
dir <- lm(direct_attack ~ CERP_all, data = afg_data)
stargazer(dir, type = "text")

# This regression estimates the relationship between military aid and indirect attacks
indir <- lm(indirect_attack ~ CERP_all, data = afg_data)
stargazer(indir, type = "text")

# This regression estimates the relationship between military aid, direct attacks
# government control of districts
dir_control <- lm(direct_attack ~ CERP_all + govt_control, data = afg_data)
stargazer(dir_control, type = "text")


# This regression estimates the relationship between military aid, indirect attacks
# and government control of districts
indir_control <- lm(indirect_attack ~ CERP_all + govt_control, data = afg_data)
stargazer(indir_control, type = "text")

# The following two regressions estimate:
# - relationship between small aid spending with direct attacks and government control
# - relationship between small aid spending with indirect attacks and government control 
small_aid_direct <- lm(direct_attack ~ CERP_small + govt_control, data = afg_data)
small_aid_indirect <- lm(indirect_attack ~ CERP_small + govt_control, data = afg_data)
stargazer(small_aid_direct, type = "text")
stargazer(small_aid_indirect, type = "text")

# The following two regressions estiamte:
# - relationship between large aid spending with direct attacks and government control
# - relationship between large aid spending with indirect attacks and government control
large_aid_direct <- lm(direct_attack ~ CERP_large + govt_control, data = afg_data)
large_aid_indirect <- lm(indirect_attack ~ CERP_large + govt_control, data = afg_data)
stargazer(large_aid_direct, type = "text")
stargazer(large_aid_indirect, type = "text")

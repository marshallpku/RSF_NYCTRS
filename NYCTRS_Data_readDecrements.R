# This script constructs mortality tables for the NCTSERS model. 

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread0
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")
options(dplyr.print_min = 60) # default is 10

# source("Functions.R")


# Comments on data issues in experience study 2015 ####
 # consistency issue:
 # why assumed probability != expected # / Total Exposed



#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/RawMaterials/"
file_name <- "2015 FAA Experience Study Discussion through June 30, 2015_Part 3 of 3 TRS only converted by Nuance.xlsx" 
file_path <- paste0(dir_data, file_name)
file_path



## 1. Mortality for Service Retirees ####

# - Assumed mortality rates are not provided.
# - Rates are calculated as "expected death under current assumption" (5) / "Total Exposure" (3)
# - Use 4-year period experience
# - Data: Male in Sheet3, female in Sheet4
# - Output variables:
#      - qxm_servRet_male
#      - qxm_servRet_female


get_mort1 <- function(data_sheet, data_range, rateName, file = file_path) {
  
	read_excel(file, sheet = data_sheet, range = data_range) %>% 
		select(age = 1, exposed = 3, death_assumed = 6) %>% 
  	mutate(!!rateName := death_assumed / exposed,
  				 age = str_extract(age, "\\d+") %>% as.numeric) %>% 
  	select(age, !!rateName)
} 


df_qxm_servRet <- 
		left_join(get_mort1("Sheet3", "c10:j70", "qxm_servRet_male"),
							get_mort1("Sheet4", "c10:j70", "qxm_servRet_female")
		)
df_qxm_servRet


## 2. Mortality for disabiltiy retirees ####

# - Assumed mortality rates are not provided.
# - Rates are calculated as "expected death under current assumption" (5) / "Total Exposure" (3)
# - Use 4-year period experience
# - Data: Male in Sheet10, female in Sheet11
# - Output variables:
#      - qxm_servRet_male
#      - qxm_servRet_female


df_qxm_disbRet <- 
	left_join(get_mort1("Sheet10", "c10:j70", "qxm_disbRet_male"),
						get_mort1("Sheet11", "c10:j70", "qxm_disbRet_female")
	)
df_qxm_disbRet



## 3. Mortality (ordinary) for active members ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet17, female in Sheet18
# - Output variables:
#      - qxm_act_male
#      - qxm_act_female


# df_qxm_disbRet <- 
# 	left_join(get_mort1("Sheet10", "c10:j70", "qxm_disbRet_male"),
# 						get_mort1("Sheet11", "c10:j70", "qxm_disbRet_female")
# 	)
# df_qxm_disbRet
















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

source("Functions.R")


#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/RawMaterials/"
file_name <- "2015 FAA Experience Study Discussion through June 30, 2015_Part 3 of 3 TRS only converted by Nuance.xlsx" 
file_path <- paste0(dir_data, file_name)
file_path



# MortaService

data_sheet <- "Sheet2"
data_range <- "c10:j70"
rateName   <- "qxm_servRet_4y"

get_mort1 <- function(data_sheet, data_range, rateName, file = file_path) {
  
	read_excel(file, sheet = data_sheet, range = data_range) %>% 
		select(age = 1, exposed = 3, death_proposed = 6) %>% 
  	mutate(!!rateName := death_proposed / exposed,
  				 age = str_extract(age, "\\d+") %>% as.numeric) %>% 
  	select(age, !!rateName)
} 
	

	
get_mort1("Sheet2", "c10:j70", "qxm_servRet_4y")	
get_mort1("Sheet5", "c10:j70", "qxm_servRet_10y")

get_mort1("Sheet9", "c10:j70", "qxm_disb_4y")	
get_mort1("Sheet12", "c10:j70", "qxm_disb_10y")


# consistency issue:
# why assumed probability != expected # / Total Exposed














# Import mortality data
data_raw_tot <- read_excel("Data_inputs/RP2014/research-2014-rp-mort-tab-rates-exposure.xlsx", sheet = "Total Dataset", skip = 3)[, c(1,2, 3, 4, 6, 7, 8)] 
data_raw_wc  <- read_excel("Data_inputs/RP2014/research-2014-rp-mort-tab-rates-exposure.xlsx", sheet = "White Collar", skip = 3)[, -4] # exclude an empty column

names(data_raw_tot) <- c("age", "qxm.employee.M", "qxm.healthyRet.M", "qxm.disbRet.M", "qxm.employee.F", "qxm.healthyRet.F", "qxm.disbRet.F" )
names(data_raw_wc)  <- c("age", "qxm.wcEmployee.M", "qxm.wcHealthyRet.M", "qxm.wcEmployee.F", "qxm.wcHealthyRet.F")

data_raw <- left_join(data_raw_wc, data_raw_tot)  


# Import projection scale (scale BB-2D)

data_scale_M <- read_excel("Data_inputs/RP2014/research-2015-mort-imp-scale-rates.xlsx", sheet = "Male", skip = 1) %>% 
                filter(!is.na(Age)) %>% 
                mutate(Age = 20:120, 
                       gender = "M")
names(data_scale_M) <- c("age",1951:2030,"gender")


data_scale_F <- read_excel("Data_inputs/RP2014/research-2015-mort-imp-scale-rates.xlsx", sheet = "Female", skip = 1) %>%
                filter(!is.na(Age)) %>% 
                mutate(Age = 20:120, 
                       gender = "F")
names(data_scale_F) <- c("age",1951:2030, "gender")


# Expand the scales to 1915-2164
 # 1915: the year when a 120-year old retiree in 2015 was at age 20. 
 # 2235: the year when a 20-year old new entrant in 2115 will be at age 120.
 # The scale BB-2D covers year 1951-2030. Years before 1951 use the scale for 1951, and years after 2030 use the scale for 2030. 




#*********************************************************************************************************
#                      ## Construct Projected Mortality table ####
#*********************************************************************************************************

# Transform data to long format
data_raw %<>% gather(type, qxm, -age) %>% mutate(year = 2014, gender = str_sub(type, -1))
data_scale_M %<>% gather(year.match, scale.M, -age, -gender) %>% mutate(year.match = as.numeric((year.match)))
data_scale_F %<>% gather(year.match, scale.F, -age, -gender) %>% mutate(year.match = as.numeric((year.match)))



# data_scale_F %<>% mutate(scale.F = 0) 
# data_scale_M %<>% mutate(scale.M = 0)


# Creat data table: age x year x type
# mortality <- expand.grid(year = 1951:2030, age = 20:120, type = levels(data_raw$type)) %>% 
#   mutate(gender = str_sub(type, -1))

mortality_RP2014 <- expand.grid(year = 1915:2235, age = 20:120, type = unique(data_raw$type)) %>% 
  mutate(gender     = str_sub(type, -1),
         year.match = ifelse(year < 1951, 1951, ifelse(year>2030, 2030, year)))




# Calculate projected mortality
mortality_RP2014 %<>% left_join(data_raw) %>% 
  left_join(data_scale_M) %>% 
  left_join(data_scale_F) %>% 
  mutate(scale = ifelse(gender == "M", scale.M, scale.F)) %>% 
  group_by(type, age) %>% 
  mutate(
    qxm_proj = ifelse(year >= 2014, qxm[year == 2014] *  cumprod(ifelse(year <= 2014, 1, 1 - scale)), NA),
    qxm_proj = ifelse(year < 2014,  qxm[year == 2014] * lead(order_by(-year, cumprod(ifelse(year > 2014, 1, 1/(1 - scale))))), qxm_proj)
    ) %>% 
  select(year, age, type, gender, qxm_proj)


# Spot check the results
df1 <- mortality_RP2014 %>% filter(type == "qxm.employee.M") %>% ungroup %>% 
              select(year, age, type, qxm_proj) %>% filter(age == 20)
df1

save(mortality_RP2014, file = "Data_inputs/NCTSERS_RP2014.RData")
load("Data_inputs/NCTSERS_RP2014.RData")

mortality_RP2014





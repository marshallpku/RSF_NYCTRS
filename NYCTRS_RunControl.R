

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


# Temporary: model parameters

tier_select = "t4a"

Global_paramlist <- list(
  nsim = 10,
	
	max_retAge = 70,
	min_retAge = 55, 
	
	max_age = 101,
	min_age = 20,
	min_ea  = 20,
	max_ea  = 68,
	
	init_year = 2015,
	nyear     = 10
)

paramlist <- list(
	                runname = "t4a",
	                # tier_select = "t4a",
									
									# benefit parameters
									fasyears = 3,
									cola = 0.015,
									
									age_vben = 62,
									
									# Return assumptions
									return_type = "simple",
									ir.mean = 0.0772,
								  ir.sd   = 0.12,
									
									# Economic assumptions
									i = 0.07,
									v = 1/(1 + 0.7),
									infl = 0.03,
									
									# Actuarial methods and assumptions
									actuarial_method = "EAN.CP",
									startingSalgrowth = 0.03,
									
									# Model assumptions
									no_entrants = FALSE,
									wf_growth = 0
  									
									) ##TIER


Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)





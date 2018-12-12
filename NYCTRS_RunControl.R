

rm(list = ls())
suppressMessages(gc())

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

# Colors
{
RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
								 RIG.orange,
								 RIG.purple,
								 RIG.green ,
								 RIG.blue,
								 RIG.yellow.dark)


RIG.theme <- function(){
	theme(panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_blank(),
				panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
				plot.title=element_text(hjust=0.5),
				plot.subtitle=element_text(hjust=0.5),
				plot.caption=element_text(hjust=0, size = 9))
}
}



#### Model Parameters ####
#********************************************************************************
folder_run <- "."
filename_RunControl <- dir(folder_run, pattern = "^RunControl")
path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)

# Import global parameters
runList <- read_excel(path_RunControl, sheet="params", skip  = 3) %>% filter(!is.na(runname), include == 1)
runList

# Import return scenarios
returnScenarios <- read_excel(path_RunControl, sheet="returns", skip = 0) %>% filter(!is.na(scenario))

# Import global parameters
Global_paramlist <- read_excel(path_RunControl, sheet="GlobalParams") %>% filter(!is.na(init_year)) %>% 
	as.list


#### Run Models and Save  ####
#********************************************************************************


folder_save <- "Results/"



#####  Run Model ####
#*********************************************************************************************************


for(runName in runList$runname ){
	
	# runName <- runList$runname
	
	paramlist <- get_parmsList(runList, runName)
	
	paramlist$simTiers <- "separate"  # "joint"(defult) or "separate"
	
	if(paramlist$nyear.override != 0) Global_paramlist$nyear <- paramlist$nyear.override
	
	

	# Investment returns
	paramlist$seed <- 1234
	
	
	paramlist$v <- 1/(1 + paramlist$i)
	
	Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
	Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)
	
	Global_paramlist$tier_select = "t4a"
	tier_select = "t4a"
	
  # paramlist$no_entrants <- TRUE
	

	# if(paramlist$tier == "sumTiers"){
	# 	source("NCTSERS_0_Master_allTiers.R")
	# 	save(outputs_list, file = paste0(folder_save, "Outputs_",  paramlist$tier, "_", runName, ".RData"))
	# 	
	# } else {
		#Tier_select <- paramlist$tier
		
	source("NYCTRS_0_Master_singleTier.R")
	# save(outputs_list, file = paste0(folder_save, "Outputs_",  paramlist$tier,"_", runName, ".RData"))
	
	#}
	
}



# # Temporary: model parameters
# 
# tier_select = "t4a"
# 
# Global_paramlist <- list(
# 	ncore = 6,
#   nsim = 2000,
# 	
# 	max_retAge = 70,
# 	min_retAge = 55, 
# 	
# 	max_age = 101,
# 	min_age = 20,
# 	min_ea  = 20,
# 	max_ea  = 68,
# 	
# 	init_year = 2016,
# 	nyear     = 30
# )
# 
# paramlist <- list(
# 	                runname = "t4a",
# 	                # tier_select = "t4a",
# 									
# 									# benefit parameters
# 									fasyears = 3,
# 									cola = 0.015,
# 									
# 									age_vben = 62,
# 									v.year   = 5,
# 									
# 									# Return assumptions
# 									return_type = "simple",
# 									return_scenario = NULL, 
# 									ir.mean = 0.0772,
# 								  ir.sd   = 0.12,
# 									
# 									# Economic assumptions
# 									i = 0.07,
# 									infl = 0.025,
# 									
# 									# Actuarial methods and assumptions
# 									actuarial_method = "EAN.CD",
# 									startingSalgrowth = 0.03,
# 									
# 									# Funding policy
# 							    s.year = 6,
# 									s.upper = 1.2, 
# 									s.lower = 0.8,
# 									smooth_method = "method1",
# 									
# 									amort_method = "cd",
# 									amort_type   = "closed",
# 									m = 14,
# 									salgrowth_amort = 0.03,
# 									
# 									ConPolicy = "ADC",
# 									nonNegC   = TRUE,
# 									EEC_fixed = TRUE,
# 									
# 									#EEC_rate = 0.04,
# 									
# 									
# 								  ## Initial Funding:
# 									init_MA_type = "AL_pct",  # need to make values more straightforward
# 									init_AA_type = "AL_pct",  # need to make values more straightforward
# 									MA_0_pct = 0.60,
# 									AA_0_pct = 0.60,
# 									MA_0 = 0,
# 									AA_0 = 0,
# 									
# 									
# 									## TDA
# 									TDA_on = TRUE,
# 									i.TDAfixed = 0.072,
# 									init_MA_TDA_type   = "MA_pct", # "preset" 
# 									init_MA_TDA_pct    = 0.4, # TDA asset as a % of QPP market value assets
# 									init_MA_TDA_preset = 0,
# 									s.year.TDA = 5,
# 								
# 									# Model assumptions
# 									no_entrants = FALSE,
# 									wf_growth = 0,
# 									
# 									useAVamort = T,
# 									useAVunrecReturn = T,
# 									
# 									estInitTerm = TRUE
# 									
# 									) ##TIER






# Final version of single tier model


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
	theme(panel.grid.major.x = element_line(size = 0.3, color = "gray90"), #element_blank(),
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
	
	
	
	paramlist$seed <- 1234 # For generating investment returns
	
	paramlist$v <- 1/(1 + paramlist$i)
	
	Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
	Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)
	
	Global_paramlist$tier_select = "t4a"
	tier_select = "t4a"
	
	source("NYCTRS_0_Master_singleTier.R")
	save(outputs_list, file = paste0(folder_save, "Outputs_",  paramlist$tier,"_", runName, ".RData"))

}













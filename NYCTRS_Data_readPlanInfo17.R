# Load the following plan information of NYCTRS:
 # Schedule of amortization payments for existing UAALs 
 # Schedule of recognizing unrecognized investment gains/losses due to asset smoothing. 
 # (Tier specific information)





#*********************************************************************************************************
#                      ## Global settings  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/"



#*********************************************************************************************************
#                      ## Initial amortization payments ####
#*********************************************************************************************************


init_amort_raw <- read_xlsx(paste0(dir_data, "NYCTRS_Planinfo_AV2017.xlsx"), sheet = "Init_amort", range = "C7:K11")
init_amort_raw



#*********************************************************************************************************
#                      ## Unrecognized investment gains/losses  ####
#*********************************************************************************************************


init_unrecReturns.unadj <- read_xlsx(paste0(dir_data, "NYCTRS_Planinfo_AV2017.xlsx"), sheet = "Init_unrecReturn", range = "C6:D11")
init_unrecReturns.unadj


#*********************************************************************************************************
#                      ## Save Data ####
#*********************************************************************************************************

save(init_amort_raw,
		 init_unrecReturns.unadj,
		 file = paste0(dir_data, "Data_planInfo17.RData"))

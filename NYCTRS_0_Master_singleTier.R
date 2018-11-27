

suppressMessages(gc())

# tier_select <- paramlist$tier

#*********************************************************************************************************
# 1.1 Load data ####
#*********************************************************************************************************

# # Load plan information
# source("NYCTRS_Data_readPlanInfo17.R")
# 
# # Load demographics data in CAFR 2017
# source("NYCTRS_Data_readDemoCAFR17.R")
# 
# # Load decrement tables and salary scales in Experience Study 2015
# source("NYCTRS_Data_readDecrements.R")
# 
# # Load Large plans data for estimation of yos distributions
# source("NYCTRS_Data_demoLargePlans.R")
# 
# 
# # Imputation of yos distributions for CAFR 2017 active members data
# source("NYCTRS_Data_ImputationActives.R")
# 
# # Construct member data
# source("NYCTRS_Data_memberData_CAFR2017.R")

dir_data <- "Inputs_data/"

load(paste0(dir_data, "Data_planInfo17.RData"))
load(paste0(dir_data, "Data_initDemographics_CAFR17.RData"))
load(paste0(dir_data, "Data_ES2015.RData"))




# Creat data for contingent retirement beneficiaries. 
# init_beneficiaries_all %<>% filter(age >= 25) 

# pct.init.ret.la <-  0.75
# pct.init.ret.ca  <- 1 - pct.init.ret.la
# 
# pct.init.disb.la <-  1
# pct.init.disb.ca  <- 1 - pct.init.disb.la
# 
# init_retirees.la_all <- init_retirees_all %>%
#   mutate(nretirees.la = nretirees * pct.init.ret.la) %>% 
#   select(-nretirees)
# 
# init_retirees.ca_all <- init_retirees_all %>%
#   mutate(nretirees.ca = nretirees * pct.init.ret.ca) %>% 
#   select(-nretirees)
# 
# init_disb.la_all <- init_disb_all %>%
#   mutate(ndisb.la = ndisb * pct.init.disb.la) %>% 
#   select(-ndisb)
# 
# init_disb.ca_all <- init_disb_all %>%
#   mutate(ndisb.ca = ndisb * pct.init.disb.ca) %>% 
#   select(-ndisb)



#*********************************************************************************************************
# 1.2 Create decrement tables ####
#*********************************************************************************************************

# Decrement tables
source("NYCTRS_Model_Decrements.R")

decrement_model <- get_decrements(tier_select)

# list.decrements      <- get_decrements(Tier_select)
# decrement.model      <- list.decrements$decrement.model
# mortality.post.model <- list.decrements$mortality.post.model



#**********************************************
##   Modify initial data ####
#**********************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(nbeneficiaries = 0)
 # init_terminated_all %<>% mutate(nterm = 0)


## Exclude initial terms with ea < 20: Data_population, line 504 
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


# ## Exclude the initial amortization basis when testing the program.
# if(!paramlist$useAVamort) init_amort_raw %<>% mutate(amount.annual = 0) 



#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("NYCTRS_Model_InvReturns.R")
i.r <- gen_returns()

#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))




#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

source("NYCTRS_Model_PrepData.R")

salary <- get_salary_proc()

benefit_servRet   <- get_benefit_servRet(init_servRet)
benefit_disbRet   <- get_benefit_disbRet(init_disbRet)
benefit_survivors <- get_benefit_survivors(init_survivors)

init_pop <- get_initPop()

entrants_dist <- get_entrantsDist(init_actives)

# Detective work
#init_pop$actives <- 0
#init_pop$servRet <- 0


# salary
# benefit_servRet
# benefit_disbRet
# benefit_survivors
# initPop
# entrants_dist

# salary        <- get_salary_proc(Tier_select)
# benefit       <- get_benefit_tier(Tier_select)
# benefit.disb  <- get_benefit.disb_tier(Tier_select)
# init_pop      <- get_initPop_tier(Tier_select)
# entrants_dist <- get_entrantsDist_tier(Tier_select)


#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("NYCTRS_Model_Demographics_singleTier.R")
invisible(gc())
pop <- get_Population()



 
# #*********************************************************************************************************
# # 3. Actuarial liabilities and benefits for contingent annuitants and survivors ####
# #*********************************************************************************************************
# source("PSERS_Model_ContingentAnnuity.R")
# 
# # For service retirement
# liab.ca <- get_contingentAnnuity(Tier_select, 
#                                  tier.param[Tier_select, "factor.ca"],
#                                  min(paramlist$range_age.r):100, 
#                                  apply_reduction = TRUE)
# 
# # For disability benefit
# range_age.disb <-  min(paramlist$range_age):100   # max(paramlist$range_age.r)
# liab.disb.ca <- get_contingentAnnuity(Tier_select, 
#                                       tier.param[Tier_select, "factor.ca.disb"],
#                                       range_age.disb, 
#                                       apply_reduction = FALSE) %>% 
#                 rename(age.disb = age.r)
# 
# 
# 

#*********************************************************************************************************
# 4. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************

source("NYCTRS_Model_IndivLiab.R")
invisible(gc())

liab <- get_indivLab(tier_select)





#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("NYCTRS_Model_AggLiab.R")
invisible(gc())

AggLiab <- get_AggLiab(tier_select,
                       liab,
                       #liab.ca,
                       #liab.disb.ca,
                       pop)





#***************************************************************
## calibration: Initial vested who are not in pay status  
#***************************************************************

# Assume the PVFB for initial vestees are paid up through out the next 50 years. 
# Based on method used in PSERS model. 

if (paramlist$estInitTerm){
AL.init.v <-  2000000000 # AV2016 pdf p17


df_init.vested <- data.frame(
	year = 1:51 + (Global_paramlist$init_year - 1),
	B.init.v.yearsum = c(0, amort_cd(AL.init.v, paramlist$i, 50, TRUE))) %>% 
	mutate(ALx.init.v.yearsum = ifelse(year == Global_paramlist$init_year, AL.init.v, 0))

df_init.vested


for(i_v in 2:nrow(df_init.vested)){
	df_init.vested$ALx.init.v.yearsum[i_v] <- 
		with(df_init.vested, (ALx.init.v.yearsum[i_v - 1] - B.init.v.yearsum[i_v - 1]) * (1 + paramlist$i))
}


AggLiab$term %<>% 
   as.data.frame() %>%
   left_join(df_init.vested, by = "year") %>%
   mutate_all(funs(na2zero)) %>%
   mutate(ALx.v.yearsum = ALx.v.yearsum + ALx.init.v.yearsum,
          B.v.yearsum   = B.v.yearsum + B.init.v.yearsum) %>%
   as.matrix
}
# 
# if(!paramlist$SepNewHires){
# 	
# 	AggLiab.sumTiers$term %<>% 
# 		as.data.frame() %>% 
# 		left_join(df_init.vested) %>% 
# 		mutate_each(funs(na2zero)) %>% 
# 		mutate(ALx.v.sum = ALx.v.sum + ALx.init.v.sum,
# 		       B.v.sum   = B.v.sum + B.init.v.sum) %>% 
# 		as.matrix
# 	
# } else {
# 	
# 	AggLiab.sumTiers.xNew$term %<>% 
# 		as.data.frame() %>% 
# 		left_join(df_init.vested) %>% 
# 		mutate_each(funs(na2zero)) %>% 
# 		mutate(ALx.v.sum = ALx.v.sum + ALx.init.v.sum,
# 		       B.v.sum   = B.v.sum + B.init.v.sum) %>% 
# 		as.matrix
# } 





#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("NYCTRS_Model_Sim_wTDA.R")
penSim_results <- run_sim(tier_select, AggLiab)




# #*********************************************************************************************************
# # 7.  Saving results ####
# #*********************************************************************************************************
# 
# outputs_list <- list(paramlist = paramlist, 
#                      Global_paramlist = Global_paramlist,
#                      results     = penSim_results)
# 
# 
# 
# #*********************************************************************************************************
# # 8. Showing results ####
# #*********************************************************************************************************
# 
# 
var_display1 <- c("Tier", "sim", "year", "FR_MA", "MA", "AA", "AL",
                  "AL.act", "AL.la", "AL.term", "PVFB", "B", "NC", "SC", "ADC", "ERC", "EEC", "NC_PR", "ERC_PR", "PR")
                  # # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  # #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB",
                  # # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca",
                  # "PR", "NC_PR", "NC","ERC")

var_display2 <- c("Tier", "sim", "year", "FR_MA", "AL.act.laca", "AL.act.v","nactives", "nla", "nterms")
                  # "n.ca.R1", "n.ca.R0S1", "nterms",
                  # "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )

var_TDA <- c("Tier", "sim", "year", "TDA_on", "i", "i.r", "i.r.wTDA", "i.leverage", "MA.TDA", "MA", "MA.TDA_QPP", "I.TDA.fixed", "I.TDA.actual")

# var_display.cali <- c("runname", "sim", "year", "FR","FR_MA", "MA", "AA", "AL",
#                       "AL.act", "AL.disb.la", "AL.term",
#                       "PVFB",
#                       "B", # "B.la", "B.ca", "B.disb.la","B.disb.ca",
#                       # "C",
#                       "NC","SC", "ERC", "EEC",
#                       "PR", "nactives", "nla",
#                       "NC_PR", "SC_PR", # "ERC_PR",
#                       "UAAL")



penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1)) %>% print
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display2)) %>% print

penSim_results %>% filter(sim == 1) %>% select(one_of(var_TDA)) %>% print


# Check geometric return
geoR <- 
	penSim_results %>% 
	filter(sim >= 1) %>% 
	group_by(sim) %>% 
	summarise(geoR_noTDA = get_geoReturn(i.r),
						geoR_TDA   = get_geoReturn(i.r.wTDA))

geoR

# Volatility drag due to TDA
geoR %>% 
	summarise(geoR_noTDA = median(geoR_noTDA),
						geoR_TDA   = median(geoR_TDA))

# # Calibration
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.cali)) %>% print
# penSim_results %>% filter(sim == 0)  %>% select(one_of(var_display.cali)) %>% print
# 
# 
# 
# #*********************************************************************************************************
# # 8. Showing risk measures ####
# #*********************************************************************************************************
# 
# df_all.stch <- penSim_results  %>% 
#   filter(sim >= 0, year <= 2045)
# 
# 
# df_all.stch %<>%   
#   select(runname, sim, year, AL, MA, EEC, PR, ERC_PR) %>% 
#   group_by(runname, sim) %>% 
#   mutate(FR_MA     = 100 * MA / AL,
#          FR40less  = cumany(FR_MA <= 40),
#          FR100more  = cumany(FR_MA >= 100),
#          FR100more2 = FR_MA >= 100,
#          ERC_high  = cumany(ERC_PR >= 50), 
#          ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
#   group_by(runname, year) %>% 
#   summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
#             FR100more = 100 * sum(FR100more, na.rm = T)/n(),
#             FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
#             ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
#             ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
#             
#             FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
#             FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
#             FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
#             FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
#             FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
#             
#             ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
#             ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
#             ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
#             ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
#             ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
#             
# 
#   ) %>% 
#   ungroup()
# 
# df_all.stch





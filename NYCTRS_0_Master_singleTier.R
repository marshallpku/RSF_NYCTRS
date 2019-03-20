

suppressMessages(gc())

cat(paramlist$runname, "\n")
# tier_select <- paramlist$tier


#*********************************************************************************************************
# 1.1 Load data (CAFR2017) ####
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
# 
# dir_data <- "Inputs_data/"
# 
# load(paste0(dir_data, "Data_planInfo17.RData"))
# load(paste0(dir_data, "Data_initDemographics_CAFR17.RData"))
# load(paste0(dir_data, "Data_ES2015.RData"))


# init_amort_raw
# init_unrecReturns.unadj


#*********************************************************************************************************
# 1.1 Load data (AV2016lag) ####
#*********************************************************************************************************

# Load plan information
source("NYCTRS_Data_readPlanInfo_AV2016.R")

# Load decrement tables and salary scales in Experience Study 2015
source("NYCTRS_Data_readDecrements.R")
 
# Load demographics data in CAFR 2017
source("NYCTRS_Data_readMemberData_AV2016.R")

# Construct member data
source("NYCTRS_Data_memberData_spread_multiTier_AV2016.R")
 

dir_data <- "Inputs_data/"
 
load(paste0(dir_data, "Data_planInfo17.RData"))
load(paste0(dir_data, "Data_ES2015.RData"))
load(paste0(dir_data, "Data_memberData_spread_wTiers_AV2016.RData"))






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
source("NYCTRS_Model_Decrements_MP2015.R")

if(paramlist$tier_Mode == "multiTier"){
	decrement_model_t4a <- get_decrements("t4a")
	decrement_model_t4b <- get_decrements("t4b")
	decrement_model_t6  <- get_decrements("t6")
}

if(paramlist$tier_Mode == "singleTier"){
	decrement_model_allTiers <- get_decrements(paramlist$singleTier_select)
} 

# save results



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
# i.r[1:5, 1:5]



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************
source("NYCTRS_Model_PrepData.R")


if(paramlist$tier_Mode == "multiTier"){
# Tier 4 basic (t4a)
	benefit_servRet_t4a   <- get_benefit_servRet(filter(init_servRet_tiers, tier == "t4a") )
	benefit_disbRet_t4a   <- get_benefit_disbRet(filter(init_disbRet_tiers, tier == "t4a") )
	
	init_pop_t4a <- get_initPop(filter(init_actives_tiers, tier == "t4a"),
	                            filter(init_servRet_tiers, tier == "t4a"),
												    	filter(init_disbRet_tiers, tier == "t4a"),
												    	filter(init_terms_tiers,   tier == "t4a")
					                )
	
	# Tier 5 55program (t4b)
	benefit_servRet_t4b   <- get_benefit_servRet(filter(init_servRet_tiers, tier == "t4b") )
	benefit_disbRet_t4b   <- get_benefit_disbRet(filter(init_disbRet_tiers, tier == "t4b") )
	
	init_pop_t4b <- get_initPop(filter(init_actives_tiers, tier == "t4b"),
															filter(init_servRet_tiers, tier == "t4b"),
															filter(init_disbRet_tiers, tier == "t4b"),
															filter(init_terms_tiers,   tier == "t4b")
	)
	
	# Tier 6 (t6)
	benefit_servRet_t6   <- get_benefit_servRet(filter(init_servRet_tiers, tier == "t6") )
	benefit_disbRet_t6   <- get_benefit_disbRet(filter(init_disbRet_tiers, tier == "t6") )
	
	init_pop_t6 <-  get_initPop(filter(init_actives_tiers, tier == "t6"),
															filter(init_servRet_tiers, tier == "t6"),
															filter(init_disbRet_tiers, tier == "t6"),
															filter(init_terms_tiers,   tier == "t6")
  )
}


# allTiers
if(paramlist$tier_Mode == "singleTier"){
	benefit_servRet_allTiers  <- get_benefit_servRet(filter(init_servRet_tiers, tier == "allTiers") )
	benefit_disbRet_allTiers  <- get_benefit_disbRet(filter(init_disbRet_tiers, tier == "allTiers") )
	
	init_pop_allTiers <-  get_initPop(filter(init_actives_tiers, tier == "allTiers"),
														      	filter(init_servRet_tiers, tier == "allTiers"),
														      	filter(init_disbRet_tiers, tier == "allTiers"),
														      	filter(init_terms_tiers,   tier == "allTiers")
	)
}

# Salary scale
salary <- get_salary_proc() # same for all tiers


# Age distribution of new hires
entrants_dist <- get_entrantsDist(filter(init_actives_tiers, tier == "t6"))






# Detective work
# init_pop$actives[,c(1, 3:51) ] <- 0
# init_pop$actives[,] <- 0
# init_pop$servRet[,] <- 0
# init_pop$disbRet[,] <- 0
# # 
# init_pop$actives
# init_pop$actives[1,26] <- 1 # ea == 20, age == 30, start_year == 2006


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

invisible(gc())


newEnt_byTier <- c(t4a = 0, t4b = 0, t6 = 1)


if(paramlist$tier_Mode == "multiTier"){
	source("NYCTRS_Model_Demographics_multiTier.R")
	
pop_multiTier <- get_Population(
	init_pop_t4a_         = init_pop_t4a,
	decrement_model_t4a_  = decrement_model_t4a,
	
	init_pop_t4b_         = init_pop_t4b,
	decrement_model_t4b_  = decrement_model_t4b,
	
	init_pop_t6_         = init_pop_t6,
	decrement_model_t6_  = decrement_model_t6,
	
	entrants_dist_    = entrants_dist)

}


if(paramlist$tier_Mode == "singleTier"){
	
	source("NYCTRS_Model_Demographics_singleTier.R")
	
	pop_allTiers <- get_Population(
		init_pop_         = init_pop_allTiers,
		entrants_dist_    = entrants_dist,
		decrement_model_  = decrement_model_allTiers)
}


# pop_multiTier$pop_t4a$la %>% filter(number.la !=0) %>% head


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



#*********************************************************************************************************
# 4. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************

source("NYCTRS_Model_IndivLiab_FlexCOLA.R")
invisible(gc())

if(paramlist$tier_Mode == "multiTier"){
	liab_t4a <- get_indivLab("t4a",
													 decrement_model_ = decrement_model_t4a,
													 salary_          = salary,
													 benefit_servRet_ = benefit_servRet_t4a,
													 benefit_disbRet_ = benefit_disbRet_t4a
													 )
	
	
	liab_t4b <- get_indivLab("t4b",
													 decrement_model_ = decrement_model_t4b,
													 salary_          = salary,
													 benefit_servRet_ = benefit_servRet_t4b,
													 benefit_disbRet_ = benefit_disbRet_t4b
	)
	
	liab_t6 <- get_indivLab("t6",
													 decrement_model_ = decrement_model_t6,
													 salary_          = salary,
													 benefit_servRet_ = benefit_servRet_t6,
													 benefit_disbRet_ = benefit_disbRet_t6
  )
}


if(paramlist$tier_Mode == "singleTier"){
	liab_allTiers <- get_indivLab(paramlist$singleTier_select,
									 				 decrement_model_ = decrement_model_allTiers,
									 				 salary_          = salary,
									 				 benefit_servRet_ = benefit_servRet_allTiers,
									 				 benefit_disbRet_ = benefit_disbRet_allTiers
	)
}



#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("NYCTRS_Model_AggLiab.R")
invisible(gc())

if(paramlist$tier_Mode == "singleTier"){
  AggLiab_allTiers <- get_AggLiab(paramlist$singleTier_select,
                                  liab_allTiers,
                                  pop_allTiers)
}

if(paramlist$tier_Mode == "multiTier"){
	AggLiab_t4a <- get_AggLiab("t4a",
														 liab_t4a,
														 pop_multiTier$pop_t4a)
	
	AggLiab_t4b <- get_AggLiab("t4b",
														 liab_t4b,
														 pop_multiTier$pop_t4b)
	
	AggLiab_t6 <- get_AggLiab("t6",
														 liab_t6,
														 pop_multiTier$pop_t6)
	
	
	AggLiab.sumTiers <- 
		get_AggLiab_sumTiers(AggLiab_t4a, AggLiab_t4b, AggLiab_t6)
	
	
}

if(paramlist$tier_Mode == "singleTier") AggLiab <- AggLiab_allTiers
if(paramlist$tier_Mode == "multiTier")  AggLiab <- AggLiab.sumTiers


#***************************************************************
## calibration: Initial vested who are not in pay status  
#***************************************************************

# Assume the PVFB for initial vestees are paid up through out the next 50 years. 
# Based on method used in PSERS model. 

if (paramlist$estInitTerm){
AL.init.v <-  1500000000 # AV2016 pdf p17


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

if(paramlist$TDA_type == "income") source("NYCTRS_Model_Sim_wTDA.R")
penSim_results <- run_sim(AggLiab)




#*********************************************************************************************************
# 7.  Saving results ####
#*********************************************************************************************************

outputs_list <- list(paramlist = paramlist,
                     Global_paramlist = Global_paramlist,
                     results     = penSim_results)

 
# #*********************************************************************************************************
# # 8. Showing results ####
# #*********************************************************************************************************


var_display1 <- c("sim", "year", "FR_MA", "MA", "AA", "AL",
                  "AL.act", "AL.la", "AL.term", "AL.disbRet", "AL.death", "PVFB", "B", "NC", "SC", "ADC", "C", "ERC", "EEC", "NC_PR", "ERC_PR", "EEC_PR", "PR", "Amort_basis", "i.r", 
									"B.la", "B.death", "B.v", "B.disbRet")
                  # # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  # #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB",
                  # # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca",
                  # "PR", "NC_PR", "NC","ERC", "B.la")

var_display2 <- c("sim", "year", "FR_MA", "AL.act.laca", "AL.act.v", "AL.act.disbRet", "nactives", "nla", "nterms", "ndisbRet")
                  # "n.ca.R1", "n.ca.R0S1", "nterms",
                  # "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )

var_display3 <- c("sim", "year", "FR_MA", "PVFB.act.laca", "PVFB.act.v", "PVFB.act.disbRet", "PVFB.act.death")



var_display3 <- c( "sim", "year", "FR_MA", "AL.act.death", "NC.death", "AL.death", "B.death")


var_TDA <- c("sim", "year", "TDA_on", "i", "i.r", "i.r.wTDA", "i.leverage", "MA.TDA", "MA", "MA.TDA_QPP", "I.TDA.fixed", "I.TDA.actual", "I.r")

# var_display.cali <- c("runname", "sim", "year", "FR","FR_MA", "MA", "AA", "AL",
#                       "AL.act", "AL.disb.la", "AL.term",
#                       "PVFB",
#                       "B", # "B.la", "B.ca", "B.disb.la","B.disb.ca",
#                       # "C",
#                       "NC","SC", "ERC", "EEC",
#                       "PR", "nactives", "nla",
#                       "NC_PR", "SC_PR", # "ERC_PR",
#                       "UAAL")


# penSim_results %>% filter(sim == -1) %>% select(year,FR_MA, MA, AL, AL.act.v, NC.v, AL.term, B.v, nterms, nactives) %>% 
# 	mutate(x = B.v == 0,
# 				 v1 = lag((AL.act.v + NC.v - B.v + AL.term) * 1.07),
# 				 v2 = AL.act.v + AL.term,
# 				 df = v1- v2
# 				 ) %>% print




penSim_results %>% filter(sim == 0) %>% select(one_of(var_display1))  %>% print
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display2)) %>% print
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display3)) %>% print

penSim_results %>% filter(sim == 1) %>% select(one_of(var_TDA)) %>% print


# Check geometric return
geoR <- 
	penSim_results %>% 
	filter(sim >= 1, year >= 2019) %>% 
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
#*********************************************************************************************************
# 8. Showing risk measures ####
#*********************************************************************************************************

df_all.stch <- penSim_results  %>%
  filter(sim >= 0, year <= 2045)


df_all.stch <- 
df_all.stch %>%
  select(runname, sim, year, AL, MA, PR, ERC_PR, ERC_noTDA_PR, i.r, i.r.wTDA) %>%
  group_by(runname, sim) %>%
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
  			 FR50less  = cumany(FR_MA <= 50),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 60),
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10)),
  			 
  			 ERC_noTDA_high  = cumany(ERC_noTDA_PR >= 60),
  			 ERC_noTDA_hike  = cumany(na2zero(ERC_noTDA_PR - lag(ERC_noTDA_PR, 5) >= 10))
  			 
  			 
  			 ) %>%
  group_by(runname, year) %>%
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
  					FR50less = 100 * sum(FR50less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
  					
  					ERC_noTDA_high = 100 * sum(ERC_noTDA_high, na.rm = T)/n(),
  					ERC_noTDA_hike = 100 * sum(ERC_noTDA_hike, na.rm = T)/n(),

            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),

            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T),
  					
  					ERC_noTDA_PR.q10 = quantile(ERC_noTDA_PR, 0.1, na.rm = T),
  					ERC_noTDA_PR.q25 = quantile(ERC_noTDA_PR, 0.25, na.rm = T),
  					ERC_noTDA_PR.q50 = quantile(ERC_noTDA_PR, 0.5, na.rm = T),
  					ERC_noTDA_PR.q75 = quantile(ERC_noTDA_PR, 0.75, na.rm = T),
  					ERC_noTDA_PR.q90 = quantile(ERC_noTDA_PR, 0.9, na.rm = T)
  					
  ) %>%
  ungroup()

df_all.stch %>% 
	filter(year %in% c(2016, seq(2020, 2045, 5)))


# Distribution of QPP and effective investment returns 





# Distribuiton of FR, ERC w/o and w/ TDA payments

# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7%"
fig_FRdist <- df_all.stch %>% # filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
	# left_join(results_all  %>%
	# 						filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>%
	# 						select(runname, year, FR_det = FR_MA)) %>%
	select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
	gather(type, value, -runname, -year) %>% 
	ggplot(aes(x = year, y = value,
						 color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")),
						 shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"))
	)) + theme_bw() + 
	# facet_grid(.~runname) +
	geom_line() + 
	geom_point(size = 2) + 
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,200)) + 
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 20)) + 
	scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
										 label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
										 label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Percent") + 
	theme(axis.text.x = element_text(size = 8)) + 
	RIG.theme()

fig_FRdist



# # Distribution of ERC ($ value)
# fig.title <- "Distribution of employer contribution rates across simulations"
# fig.subtitle <- "Assumption achieved: expected compound return = 7%"
# fig_ERCdist <- df_all.stch %>% # filter(runname %in% c("RS1.closed", "RS1.open")) %>%
# 	select(runname, year, ERC_PR.q25,
# 				                ERC_PR.q50,
# 				                ERC_PR.q75,
# 				                ERC_noTDA_PR.q25,
# 				                ERC_noTDA_PR.q50,
# 				                ERC_noTDA_PR.q75) %>%
# 	gather(Var, value, -runname, -year) %>%
# 	mutate(type = ifelse(str_detect(Var, "TDA"), "noTDA", "TDA"),
# 				 Var  = str_replace(Var, "noTDA_", "")) %>%
# 	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%
# 	ggplot(aes(x = year, y = value,
# 						 color = factor(Var, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) +
# 	facet_grid(. ~ type) +
# 	theme_bw() +
# 	geom_line() +
# 	geom_point(size = 2) +
# 	coord_cartesian(ylim = c(-20,60)) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) +
# 	scale_y_continuous(breaks = seq(-100, 100, 10)) +
# 	scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL,
# 										 label  = c("75th percentile", "50th percentile", "25th percentile")) +
# 	scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL,
# 										 label  = c("75th percentile", "50th percentile", "25th percentile")) +
# 	labs(title = fig.title,
# 			 subtitle = fig.subtitle,
# 			 x = NULL, y = "%") +
# 	theme(axis.text.x = element_text(size = 8)) +
# 	RIG.theme()
# 
# fig_ERCdist
# 
# 
# 
# 
# # Probs of low funded ratio, ERC high and ERC hike w/ and w/o TDA payouts
# 
# 
# # Risk of low funded ratio
# fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
# fig.subtitle <- "Assumption achieved; expected compound return = 8%"
# fig_FR40less <- df_all.stch %>% # filter(runname %in% c("RS1.closed", "RS1.open")) %>%
# 	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%
# 	select(runname, year, FR40less) %>%
# 	#mutate(FR40less.det = 0) %>%
# 	#gather(variable, value, -year) %>%
# 	ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() +
# 	geom_point(size = 2) + geom_line() +
# 	coord_cartesian(ylim = c(0,10)) +
# 	scale_y_continuous(breaks = seq(0,200, 2)) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) +
# 	scale_color_manual(values = c("black",RIG.red),  name = "") +
# 	scale_shape_manual(values = c(17,16),  name = "") +
# 	labs(title = fig.title,
# 			 subtitle = fig.subtitle,
# 			 x = NULL, y = "Probability (%)") +
# 	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
# 	RIG.theme()
# fig_FR40less
# 
# 
# 
# # Risk of sharp increase in ERC
# fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
# fig.subtitle <- "Assumption achieved; expected compound return = 7%"
# fig_ERChike <- df_all.stch %>%
#   select(runname, year, ERC_hike, ERC_TDA_hike) %>%
# 	#mutate(ERChike.det = 0) %>%
# 	gather(type, value, -year, -runname) %>%
# 	ggplot(aes(x = year, y = value, color = type, shape = type)) + theme_bw() +
# 	geom_point(size = 2) + geom_line() +
# 	coord_cartesian(ylim = c(0,100)) +
# 	scale_y_continuous(breaks = seq(0,200, 5)) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) +
# 	scale_color_manual(values = c("black", RIG.red),  name = "") +
# 	scale_shape_manual(values = c(17,16),  name = "") +
# 	labs(title = fig.title,
# 			 subtitle = fig.subtitle,
# 			 x = NULL, y = "Probability (%)") +
# 	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
# 	RIG.theme()
# fig_ERChike
# 
# 
# 
# 
# # Risk of sharp increase in ERC
# fig.title <- "Probability of employer contribution rising above 50% of payroll \nat any time prior to and including the given year"
# fig.subtitle <- "Assumption achieved; expected compound return = 7%"
# fig_ERChike <- df_all.stch %>%
# 	select(runname, year, ERC_high, ERC_TDA_high) %>%
# 	#mutate(ERChike.det = 0) %>%
# 	gather(type, value, -year, -runname) %>%
# 	ggplot(aes(x = year, y = value, color = type, shape = type)) + theme_bw() +
# 	geom_point(size = 2) + geom_line() +
# 	coord_cartesian(ylim = c(0,100)) +
# 	scale_y_continuous(breaks = seq(0,200, 10)) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) +
# 	scale_color_manual(values = c("black", RIG.red),  name = "") +
# 	scale_shape_manual(values = c(17,16),  name = "") +
# 	labs(title = fig.title,
# 			 subtitle = fig.subtitle,
# 			 x = NULL, y = "Probability (%)") +
# 	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
# 	RIG.theme()
# fig_ERChike













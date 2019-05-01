# Prepare figures for the first symposium 

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(xlsx)
library("btools")
library("scales")
library(gridExtra)
library(grid)
library(plotly)
library(ggfan) # fan charts using ggplot2


source("Functions.R")

options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20



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





#*****************************************************
##  Notes         ####
#*****************************************************

# Analysis 1: Impact of TDA

# Three scenarios
#  - no TDA
#  - TDA payments are amortized


# Analysis 2: Impact of funcing policy 

# Three scenarios
#  - Plan policy (TDA payments are amortized)
#  - More backloaded amortization policies: cd/cp, open/closed, 15->30
#  - One-Year-Lag-Method






#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
Outputs_folder  <- "Outputs_figures/"


save_figure <- function(fig_ob, fig_folder = Outputs_folder,  fig_type = "png", ...){
	figure_name <- deparse(substitute(fig_ob))
	ggsave(paste0(fig_folder, figure_name, ".", fig_type ), fig_ob, ... )
	
}


#*****************************************************
##  Loading data  ####
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    # if("results.t7" %in% names(outputs_list)){
    #   df_out <- bind_rows(outputs_list$results,
    #                       outputs_list$results.t7,
    #                       outputs_list$results.xt7)
    #   return(df_out)
    # } else {
    #   return(outputs_list$results)
    # }
    
    return(outputs_list$results)
    
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder) %>% select(runname, sim, year, everything())

#results_all %>% head
#results_all$runname %>% unique


#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

runs_TDA <-   c("multiTier_noTDA",
								"multiTier_TDAamortAS",
								"multiTier_TDApayout")

runs_TDA_OYLM <- c("multiTier_noTDA_OYLM",
									 "multiTier_TDAamortAS_OYLM",
								   "multiTier_TDApayout_OYLM")



runs_TDA_labels <- c("Without TDA",
										 "TDA amortized; AS",
										 "TDA not amortized")

runs_TDA_OYLM_labels <- c("Without TDA; OYLM",
										      "TDA amortized; AS; OYLM",
										      "TDA not amortized; OYLM")


runs_RS <- c("multiTier_noTDA_OYLM_low15",
						 "multiTier_TDAamortAS_OYLM_low15",
						 "multiTier_noTDA_OYLM_low30",
						 "multiTier_TDAamortAS_OYLM_low30"
						 )

runs_RS_labels <- c("15 years of low returns; w/o TDA",
										"15 years of low returns; w/ TDA",
										"30 years of low returns; w/o TDA",
										"30 years of low returns; w/ TDA"
)


runs_DF <- c("multiTier_noTDA_OYLM_DF1",
						 "multiTier_TDAamortAS_OYLM_DF1",
						 "multiTier_noTDA_OYLM_DF2",
						 "multiTier_TDAamortAS_OYLM_DF2"
)

runs_DF_labels <- c("Asset shock scenario; w/o TDA",
										"Asset shock scenario; w/ TDA",
										"Asset shock scenario 2; w/o TDA",
										"Asset shock scenario 2; w/ TDA"
										)




runs_fPolicy <-   c(
										"multiTier_O15dA6",
										"multiTier_O15pA6",
										"multiTier_O30pA6",
										"multiTier_C30dA6",
										"multiTier_C15pA6",
										"multiTier_C15dA0",
										"multiTier_C15dA6noCorridor",
										"multiTier_O30pA6_noTDA"
								    )

runs_fPolicy_labels <- c("Open amort.",
												 "Open level pct amort.",
												 "open level pct 30-year amort.",
	                       "30-year amort.",
												 "Level percent amort.",
												 "no asset smoothing",
												 "no corridor",
												 "open level pct 30-year amort; no TDA")


runs_all        <- c(runs_TDA, runs_TDA_OYLM, runs_RS, runs_DF, runs_fPolicy)
runs_all_labels <- c(runs_TDA_labels, runs_TDA_OYLM_labels, runs_RS_labels, runs_DF_labels, runs_fPolicy_labels)



# Calculate total final ERC rate for runs with DC reform (include ERC to DC in ERC.final_PR)


# in the past 5 years ERC rate
# ERC_rate_5y <- data.frame(year = 2012:2016, ERC_rate_5y = c(0.0869,0.0915, 0.0915 ,0.0998, 0.1078))
# ERC_rate_5y <- c(8.69,9.15, 9.15 ,9.98, 10.78)



#*************************************************************
##     Incorporating term cost for UFT 8.25% guarantee    ####
#*************************************************************

# - The term cost is calculated as 1.25% of the portion of TDA Fixed Return funds
#   with 8.25% gurantee.
# - The term cost is added to added to NC and ERC. 

results_all %<>% 
	mutate(NC_wtermCost  = NC  + termCost_UFT,
				 ERC_wtermCost = ERC + termCost_UFT,
				 NC_PR_wtermCost    = 100 * NC_wtermCost / PR,
				 ERC_PR_wtermCost   = 100 * ERC_wtermCost / PR
				 )



#*****************************************************
##  Computing risk measures ####
#*****************************************************

df_all.stch <- 
  results_all %>%
	filter(runname %in% runs_all) %>% 
  filter(sim >= 0, year <= 2048)


df_all.stch <- 
	df_all.stch %>%
	select(runname, sim, OYLM_on, TDA_policy, return_scenario, year, AL, MA, PR, ERC_PR, ERC_PR_wtermCost, i.r, i.r.wTDA) %>%
	group_by(runname, sim) %>%
	mutate(FR_MA     = 100 * MA / AL,
				 FR40less  = cumany(FR_MA <= 40),
				 FR50less  = cumany(FR_MA <= 50),
				 FR100more  = cumany(FR_MA >= 100),
				 FR100more2 = FR_MA >= 100,
				 ERC_high  = cumany(ERC_PR_wtermCost >= 60),
				 ERC_hike  = cumany(na2zero(ERC_PR_wtermCost - lag(ERC_PR_wtermCost, 5) >= 10)),
				 
				 
	) %>%
	group_by(runname, year) %>%
	summarize(
		
	         	OYLM = unique(OYLM_on),
	         	TDA_policy = unique(TDA_policy),
	        	return_scenario = unique(return_scenario),
		
		        FR40less = 100 * sum(FR40less, na.rm = T)/n(),
						FR50less = 100 * sum(FR50less, na.rm = T)/n(),
						FR100more = 100 * sum(FR100more, na.rm = T)/n(),
						FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
						ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
						ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
						
						FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
						FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
						FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
						FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
						FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
						
						ERC_PR.q10 = quantile(ERC_PR_wtermCost, 0.1, na.rm = T),
						ERC_PR.q25 = quantile(ERC_PR_wtermCost, 0.25, na.rm = T),
						ERC_PR.q50 = quantile(ERC_PR_wtermCost, 0.5, na.rm = T),
						ERC_PR.q75 = quantile(ERC_PR_wtermCost, 0.75, na.rm = T),
						ERC_PR.q90 = quantile(ERC_PR_wtermCost, 0.9, na.rm = T),
						
						#OYLM = unique(OYLM),
						TDA_policy = unique(TDA_policy),
						return_scenario = unique(return_scenario)
						

	) %>% 
	mutate(runname.fct = factor(runname, levels = runs_all, labels = runs_all_labels)) %>%
	arrange(runname.fct) %>% 
	ungroup() 

	

#***********************************************************************
## Q1S1: Describe the problem of pension risks as you understand them ####
#***********************************************************************

# Illustration of single runs
# Actual returns and effective returns: MA based
# Actual returns and effective returns: AA based
# Funded ratio
# ERC

# Calculate geometric return for each sim
geoR <- 
	results_all %>%
	filter(runname == "multiTier_TDAamortAS_OYLM") %>% 
	filter(sim >= 1, year >= 2019) %>% 
	group_by(sim) %>% 
	summarise(geoR_noTDA = get_geoReturn(i.r),
						geoR_TDA   = get_geoReturn(i.r.wTDA)) %>% 
	arrange(geoR_noTDA)

geoR %>% filter(is.nan(geoR_TDA))

# Volatility drag due to TDA
geoR %>% 
	summarise(geoR_noTDA = median(geoR_noTDA, na.rm = T),
						geoR_TDA   = median(geoR_TDA,   na.rm = T))




# pick two sims with geom return approx. equal to 7%  
geoR %>% filter(geoR_noTDA > 0.0695, geoR_noTDA <= 0.0705) # 476

# pick 4 sims with geom return 
geoR_qtl <- 
	geoR %>% 
	summarise(geoR_q10 = quantile(geoR_noTDA, 0.1,  na.rm = T), 
		        geoR_q25 = quantile(geoR_noTDA, 0.25, na.rm = T),
						geoR_q75 = quantile(geoR_noTDA, 0.75, na.rm = T),
						geoR_q90 = quantile(geoR_noTDA, 0.90, na.rm = T))

geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q10-0.0003, geoR_noTDA <= geoR_qtl$geoR_q10+0.0003) # 
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q25-0.0003, geoR_noTDA <= geoR_qtl$geoR_q25+0.0003) # 
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q75-0.0003, geoR_noTDA <= geoR_qtl$geoR_q75+0.0003) # 
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q90-0.0003, geoR_noTDA <= geoR_qtl$geoR_q90+0.0003) # 



# 23   912 0.06993192 0.06830289

singleRuns1 <- c(0, 275, 476)
# 424, 476, 275 
# 149, 507

df_singleRuns1 <- 
	results_all %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", sim %in% singleRuns1 ) %>% 
	select(runname, sim, year, FR_MA, ERC_PR, i.r, i.r.wTDA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_TDA_OYLM[2], labels = runs_TDA_OYLM_labels[2])) 
df_singleRuns1

	# Actual return vs effective return with TDA


fig.title    <- "Investment returns in three single simulation runs "
fig.subtitle <- " All three runs have the same 30-year geometric return of 7.0%"
fig_singleReturns1 <- 
	df_singleRuns1 %>% 
	filter(year >= 2018) %>% 
	select(runname.fct, sim, year, i.r) %>% 
	#gather(Var, value, -year,-runname.fct) %>% 
	mutate(sim = factor(sim, levels = singleRuns1, 
											     labels = c("Constant return", paste0("Sim ", singleRuns1[2]), paste0("Sim ", singleRuns1[3])))) %>%
	ggplot(aes(x = year, y = i.r*100, color = sim)) + theme_bw()+
	geom_line() + 
	geom_point() +
	geom_hline(yintercept = 7, linetype = 2)+
	scale_y_continuous(breaks = c(7, seq(-100,100, 5)) ) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black", "deepskyblue1", "firebrick1")) + 
	labs(title = fig.title   ,
			 subtitle = fig.subtitle,
			 color = NULL,
			 x = NULL, y = "Rate of return (%)") +
	RIG.theme() +
	theme(legend.position = "bottom")

	fig_singleReturns1



	
fig.title    <- "Funded ratios in three single simulation runs "
fig.subtitle <- "All three runs have the same 30-year geometric return of 7.0%"		
	
fig_TDA_singleReturns1_FR <- 
	df_singleRuns1 %>% 
	filter(year >= 2018) %>% 
	mutate(sim = factor(sim, 
											levels = singleRuns1, 
											labels = c("Constant return", paste0("Sim ", singleRuns1[2]), paste0("Sim ", singleRuns1[3])))) %>%
	
	ggplot(aes(x = year, y = FR_MA, color = factor(sim))) + theme_bw()+
	geom_line() + 
	geom_point() + 
	geom_hline(yintercept = 100, linetype = 2)+
	coord_cartesian(ylim = c(0,150)) + 
	scale_y_continuous(breaks = seq(0,200, 20)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black", "deepskyblue1", "firebrick1")) +
	labs(title =    fig.title,
			 subtitle = fig.subtitle,
			 color = NULL,
			 x = NULL, 
			 y = "Funded ratio (%)") +
	RIG.theme()
fig_TDA_singleReturns1_FR


fig.title    <- "Employer contribution rates in three single simulation runs "
fig.subtitle <- " All three runs have the same 30-year geometric return of 7.0%"		
fig_TDA_singleReturns1_ERC <- 
  df_singleRuns1 %>% 
	filter(year >= 2018) %>% 
	mutate(sim = factor(sim, 
											levels = singleRuns1, 
											labels = c("Constant return", paste0("Sim ", singleRuns1[2]), paste0("Sim ", singleRuns1[3])))) %>%
	ggplot(aes(x = year, y = ERC_PR, color = factor(sim))) + theme_bw()+
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,50)) + 
	scale_y_continuous(breaks = seq(-100,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black", "deepskyblue1", "firebrick1")) +
	labs(title =    fig.title,
			 subtitle = fig.subtitle,
			 color = NULL,
			 x = NULL, 
			 y = "Employer contribution Rate (%)") +
	RIG.theme()
fig_TDA_singleReturns1_ERC 



#***********************************************************************
## Q1S2: Describe the problem of pension risks as you understand them ####
#***********************************************************************

	
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q10-0.0003, geoR_noTDA <= geoR_qtl$geoR_q10+0.0003) # 
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q25-0.0003, geoR_noTDA <= geoR_qtl$geoR_q25+0.0003) # 
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q75-0.0003, geoR_noTDA <= geoR_qtl$geoR_q75+0.0003) # 
geoR %>% filter(geoR_noTDA > geoR_qtl$geoR_q90-0.0003, geoR_noTDA <= geoR_qtl$geoR_q90+0.0003) # 



# 23   912 0.06993192 0.06830289

singleRuns2 <- c(648, 458, 275, 1245, 1093)
# q10
# q25
# q50: 476
# q75
# q90: 1093, 1577,752

singleRuns2_geoR <- (unlist(geoR[geoR$sim %in% singleRuns2, "geoR_noTDA"])*100) %>% round(digits = 1) # %>% as.character()


df_singleRuns2 <- 
	results_all %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", sim %in% singleRuns2, year >= 2018) %>% 
	select(runname, sim, year, FR_MA, ERC_PR, i.r, i.r.wTDA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_TDA_OYLM[2], labels = runs_TDA_OYLM_labels[2])) 
# df_singleRuns2


fig.title    <- "Funded ratios \nin 5 single simulation runs with different average returns  "
fig.subtitle <- NULL		

fig_TDA_singleReturns2_FR <- 
	df_singleRuns2 %>% 
	mutate(sim = factor(sim, 
											levels = singleRuns2, 
											labels = c(paste0(singleRuns2_geoR[1],"%"),
																 paste0(singleRuns2_geoR[2],"%"), 
																 paste0(singleRuns2_geoR[3],"%"),
																 paste0(singleRuns2_geoR[4],"%"),
																 paste0(singleRuns2_geoR[5],"%")))) %>%
	ggplot(aes(x = year, y = FR_MA, color = factor(sim))) + theme_bw()+
	geom_line() + 
	geom_point() + 
	geom_hline(yintercept = 100, linetype = 2) +
	coord_cartesian(ylim = c(0,250)) +  
	scale_y_continuous(breaks = seq(0,300, 20)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("firebrick1", RIG.red, "grey60", "dodgerblue3", "deepskyblue1")) +
	labs(title =    fig.title,
			 subtitle = fig.subtitle,
			 color = NULL,
			 x = NULL, 
			 y = "Funded ratio (%)") +
	RIG.theme()
fig_TDA_singleReturns2_FR


fig.title    <- "Employer contribution rates \nin 5 single simulation runs with different average returns  "
fig.subtitle <- NULL		

fig_TDA_singleReturns2_ERC <- 
	df_singleRuns2 %>% 
	mutate(sim = factor(sim, 
											levels = singleRuns2, 
											labels = c(paste0(singleRuns2_geoR[1],"%"),
																 paste0(singleRuns2_geoR[2],"%"), 
																 paste0(singleRuns2_geoR[3],"%"),
																 paste0(singleRuns2_geoR[4],"%"),
																 paste0(singleRuns2_geoR[5],"%")))) %>%
	ggplot(aes(x = year, y = ERC_PR, color = factor(sim))) + theme_bw()+
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,70)) + 
	scale_y_continuous(breaks = seq(-100,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_color_manual(values = c("firebrick1", RIG.red, "grey60", "dodgerblue3", "deepskyblue1")) +
	labs(title =    fig.title,
			 subtitle = fig.subtitle,
			 color = "Average return",
			 x = NULL, y = "Employer contribution Rate (%)") +
	RIG.theme()
fig_TDA_singleReturns2_ERC 



	
#***********************************************************************
## Q4S1: Current policy: percentile figures for current policy ####
#***********************************************************************

# Distribution of funded ratio
df_FR_q <- 
	df_all.stch %>%
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019) %>%
	select(runname.fct, year,
				 FR.q10,
				 FR.q25,
				 FR.q50,
				 FR.q75,
				 FR.q90) %>%
	gather(Var, value, -runname.fct, -year) %>% 
	mutate(Var_q = as.numeric(str_extract(Var, "\\d+"))/100,
				 Var   = factor(Var, levels = c("FR.q90", "FR.q75", "FR.q50", "FR.q25", "FR.q10"),
				 							       labels = c("90th", "75th", "50th", "25th", "10th")))


fig.title    <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7% (before TDA transfer)"
results_all %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019, sim > 0) %>%
	select(year, sim, FR_MA) %>% 
	
	ggplot(aes(x=year,y=FR_MA)) + 
	geom_fan(intervals =c(0.5, seq(0, 80, 8)[-1]/100)) + 
	geom_line( data = df_FR_q,  mapping  = aes(x = year, y = value, linetype = Var))+
	geom_point(data = df_FR_q, mapping = aes(x = year, y = value,  shape    = Var), size = 1.8 , color = RIG.red)+
	geom_hline(yintercept = 100, linetype = 2, size = 0.5) +
	
	coord_cartesian(ylim = c(0,250)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(-100, 300, 20)) +
	
	scale_fill_gradient(low="red", high="pink") +
	scale_linetype_manual(values = c(1, 1, 1, 1, 1 ) ) +
	scale_shape_manual(values = c(1,16, 15, 17, 2, 19, 20)) +
	
	labs(title = fig.title,
			 subtitle = fig.subtitle,
		   linetype = "Percentiles",
			 shape    = "Percentiles",
			 x = NULL,
			 y = "Funded ratio (%)") +
	guides(fill = FALSE) + 
	theme_bw() +
	RIG.theme()



# Distribution of employer contribution

df_ERC_q <- 
	df_all.stch %>%
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019) %>%
	select(runname.fct, year,
				 ERC_PR.q10,
				 ERC_PR.q25,
				 ERC_PR.q50,
				 ERC_PR.q75,
				 ERC_PR.q90) %>%
	gather(Var, value, -runname.fct, -year) %>% 
	mutate(Var_q = as.numeric(str_extract(Var, "\\d+"))/100,
				 Var   = factor(Var, levels = c("ERC_PR.q90", 
				 															  "ERC_PR.q75", 
				 															  "ERC_PR.q50", 
				 															  "ERC_PR.q25", 
				 															  "ERC_PR.q10"),
				 							       labels = c("90th", "75th", "50th", "25th", "10th")))


fig.title    <- "Distribution of employer contribution rates across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7% (before TDA transfer)"
results_all %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019, sim > 0) %>%
	select(year, sim, ERC_PR_wtermCost) %>% 
	
	ggplot(aes(x=year,y=ERC_PR_wtermCost)) + 
	geom_fan(intervals = c(0.5, seq(0, 80, 8)[-1]/100)) + 
	geom_line( data = df_ERC_q,  mapping  = aes(x = year, y = value, linetype = Var))+
	geom_point(data = df_ERC_q,  mapping  = aes(x = year, y = value,  shape   = Var), size = 1.8 , color = RIG.red)+
	geom_hline(yintercept = 100, linetype = 2, size = 0.5) +
	
	coord_cartesian(ylim = c(0,60)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(-100, 300, 10)) +
	
	scale_fill_gradient(low="red", high="pink") +
	scale_linetype_manual(values = c(1, 1, 1, 1, 1 ) ) +
	scale_shape_manual(values = c(1,16, 15, 17, 2, 19, 20)) +
	
	labs(title = fig.title,
		   subtitle = fig.subtitle,
		   linetype = "Percentiles",
			 shape    = "Percentiles",
			 x = NULL,
			 y = "Employer contribution as a percentage of payroll (%)") +
	guides(fill = FALSE) + 
	theme_bw() +
	RIG.theme()




#***********************************************************************
## Q4S2: Impact of TDA: Deterministic asset shock  ####
#***********************************************************************


results_all %>% 
	filter(runname %in% runs_DF, sim == 1, year %in% c(2019, 2020, 2021, 2048)) %>% 
	select(runname, year, FR_MA, ERC_PR, AA, MA, ERC_PR) 

fig.title <- "TRS funded ratio under \nhypothetical asset shock scenario"
fig.subtitle <- NULL

fig_shock1_FR <- 
	results_all %>% 
	filter(runname %in% runs_DF[1:2], sim == 1, year >= 2019) %>% 
	select(runname, year, FR_MA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[1:2], labels = runs_DF_labels[1:2])) %>% 
	ggplot(aes(x = year,
						 y = FR_MA,
						 color = runname.fct,
						 shape = runname.fct)) + theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,100)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 500, 20)) +
	scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Funded ratio (%)") +
	theme(axis.text.x = element_text(size = 8),
				legend.position = "bottom",
				legend.key.width = unit(0.5, "inch" )) +
	RIG.theme()
fig_shock1_FR


fig.title <- "TRS employer contribution rate under \nhypothetical asset shock scenario"
fig.subtitle <- NULL
fig_shock1_ERC <- 
	results_all %>% 
	filter(runname %in% runs_DF[1:2], sim == 1, year >= 2019) %>% 
	select(runname, year, ERC_PR) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[1:2], labels = runs_DF_labels[1:2])) %>% 
	ggplot(aes(x = year,
						 y = ERC_PR,
						 color = runname.fct,
						 shape = runname.fct)) + theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	coord_cartesian(ylim = c(0,60)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 500, 10)) +
	scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Employer contribution as a percentage of payroll") +
	theme(axis.text.x = element_text(size = 8),
				legend.position = "bottom",
				legend.key.width = unit(0.5, "inch" )) +
	RIG.theme()
fig_shock1_ERC





#***********************************************************************
## Q4S3: Impact of TDA: Risk measures  ####
#***********************************************************************


runs_RS1 <-   c("multiTier_noTDA_OYLM",
								"multiTier_TDAamortAS_OYLM",
								"multiTier_noTDA_OYLM_low15",
								"multiTier_TDAamortAS_OYLM_low15"
								#"multiTier_noTDA_OYLM_low30",
								#"multiTier_TDAamortAS_OYLM_low30"
)

runs_RS1_label <- c("t4a_noTDA_OYLM",
										"t4a_TDAamortAS_OYLM",
										
										"15 years of low returns; w/o TDA",
										"15 years of low returns; w/ TDA"
										#"30 years of low returns; w/o TDA",
										#"30 years of low returns; w/ TDA"
)


fig.title <- "Probability of funded ratio below 40% in any year up to the given year\nunder alternative return scenarios"
fig.subtitle <- NULL#"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_TDA_FR40less_2fig <- df_all.stch %>% 
	filter(runname %in% runs_RS1) %>% 
	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
	select(runname.fct, TDA_policy, return_scenario, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	# gather(variable, value, -year, -runname.fct, -TDA_policy, -return_scenario) %>% 
	mutate(
		#      variable = factor(variable, 
		# 											 levels = c("FR40less", "FR50less"),
		# 											 labels = c("Funded ratio below 40%", "Funded ratio below 50%")),
		return_scenario = factor(return_scenario, 
														 levels = c("RS1", "RS_15low"),
														 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
		TDA_policy = factor(TDA_policy, 
												levels = c("noTDA", "TDAamortAS"),
												labels = c("Without TDA", "With TDA"))
		
		
	) %>% 
	ggplot(aes(x = year, y = FR40less, color =TDA_policy, shape = TDA_policy)) + theme_bw() + 
	facet_grid(.~ return_scenario) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,60)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16,15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_TDA_FR40less_2fig



#***********************************************************************
## Q4S4: Funding policies   ####
#***********************************************************************


runs_fPolicy1 <- c(
	"multiTier_TDAamortAS_OYLM",
	"multiTier_TDAamortAS",
	"multiTier_C15dA0",
	"multiTier_C15dA6noCorridor",
	"multiTier_O15dA6",
	"multiTier_O15pA6",
	"multiTier_O30pA6",
	"multiTier_O30pA6_noTDA",
	"multiTier_C30dA6",
	"multiTier_C15pA6",
	"multiTier_noTDA_OYLM"
	
)

runs_fPolicy_labels1 <- c("TRS policy",
													"No One-Year-Lag-Method",
													"No asset smoothing",
													"No corridor",
													"Open amort.",
													"Open level pct amort.",
													"Open level pct 30-year amort.",
													"Open level pct 30-year amort; noTDA",
													"30-year amort.",
													"Level percent amort.",
													"No TDA"
)


fig.title <- "Probability of funded ratio below 40% or 50% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_policy_lowFR <- 
	df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	gather(variable, value, -year, -runname.fct) %>% 
	mutate(variable = factor(variable, 
													 levels = c("FR40less", "FR50less"),
													 labels = c("Funded ratio below 40%", "Funded ratio below 50%"))) %>% 
	ggplot(aes(x = year, y = value, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	facet_grid(.~ variable) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,65)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40",  "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16, 15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_policy_lowFR 


# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_policy_ERChike <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, ERC_hike) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40",  "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_policy_ERChike


# Figure: Risk of high ERC (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_policy_ERChigh <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, ERC_high) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,50)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40", "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_policy_ERChigh 



#*****************************************************
## Analysis 1 Impact of Funding policy ####
#*****************************************************

runs_fPolicy1 <- c(
	"multiTier_TDAamortAS_OYLM",
	"multiTier_TDAamortAS",
	"multiTier_C15dA0",
	"multiTier_C15dA6noCorridor",
	"multiTier_O15dA6",
	"multiTier_O15pA6",
	"multiTier_O30pA6",
	"multiTier_O30pA6_noTDA",
	"multiTier_C30dA6",
	"multiTier_C15pA6",
	"multiTier_noTDA_OYLM"
	
)

runs_fPolicy_labels1 <- c("TRS policy",
												 "No One-Year-Lag-Method",
												 "No asset smoothing",
												 "No corridor",
	                       "Open amort.",
												 "Open level pct amort.",
												 "Open level pct 30-year amort.",
												 "Open level pct 30-year amort; noTDA",
												 "30-year amort.",
												 "Level percent amort.",
												 "No TDA"
												 )


fig.title <- "Probability of funded ratio below 40% or 50% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_policy_lowFR <- 
df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	gather(variable, value, -year, -runname.fct) %>% 
	mutate(variable = factor(variable, 
													 levels = c("FR40less", "FR50less"),
													 labels = c("Funded ratio below 40%", "Funded ratio below 50%"))) %>% 
	ggplot(aes(x = year, y = value, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	facet_grid(.~ variable) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,65)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40",  "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16, 15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_policy_lowFR 


# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_policy_ERChike <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, ERC_hike) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40",  "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_policy_ERChike


# Figure: Risk of high ERC (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_policy_ERChigh <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, ERC_high) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,50)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40", "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_policy_ERChigh 



save_figure(fig_policy_lowFR,   w = 2*6, h = 0.85*6)
save_figure(fig_policy_ERChike, w = 2*4.5, h = 1.3*4.5 )
save_figure(fig_policy_ERChigh, w = 2*4.5, h = 1.3*4.5 )



# Bar plots for impact of funding policy

df_all.stch %>% 
  filter(runname %in% runs_fPolicy1[1:7], year == 2048) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, ERC_high) %>% 
	ggplot(aes(x = runname.fct, y = ERC_high)) + 
	geom_bar(stat = "identity")
	

df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7], year == 2048) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, ERC_hike) %>% 
	ggplot(aes(x = runname.fct, y = ERC_hike)) + 
	geom_bar(stat = "identity")


df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:7], year == 2048) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:7], labels = runs_fPolicy_labels1[1:7])) %>% 
	select(runname.fct, year, FR40less) %>% 
	ggplot(aes(x = runname.fct, y = FR40less)) + 
	geom_bar(stat = "identity")



#**************************************************************
## Analysis 1.1 Impact of Funding policy, focused analysis ####
#**************************************************************


fig.title <- "Probability of funded ratio below 40% \nin any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
fig_lowFR.fPolicy <- 
	df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 8, 11)], year >=2018) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(8, 11, 1)], 
															labels = c("No TDA,\n Open 30-year amort.",
																				 "No TDA,\n Closed 15-year amort.",
																				 "TDA, \nClosed 15-year amort. \n(TRS policy)"))) %>% 
	select(runname.fct, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	# gather(variable, value, -year, -runname.fct) %>% 
	# mutate(variable = factor(variable, 
	# 												 levels = c("FR40less", "FR50less"),
	# 												 labels = c("Funded ratio below 40%", "Funded ratio below 50%"))) %>% 
	ggplot(aes(x = year, y = FR40less, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	#facet_grid(.~ variable) + 
	geom_point(size = 1.9) + geom_line() + 
	coord_cartesian(ylim = c(0,35)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40",  "black", "grey60",  RIG.blue, RIG.red, RIG.green)) + 
	scale_shape_manual(values = c(1,16, 15, 21, 18, 19, 20)) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, 
			 y = "Probability (%)",
			 color = NULL,
			 shape = NULL) + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	theme(legend.position = c(0.01,0.99),
				legend.justification = c(0,1),
				legend.background = element_rect(fill="white", 
																				 size=0.5, linetype="solid", color = "grey70") ) + 
	RIG.theme()
fig_lowFR.fPolicy


# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
fig_ERChike.fPolicy <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 8, 11)], year >= 2018) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(8, 11, 1)], 
															labels = c("No TDA,\n Open 30-year amort.",
																				 "No TDA,\n Closed 15-year amort.",
																				 "TDA, \nClosed 15-year amort. \n(TRS policy)"))) %>%	select(runname.fct, year, ERC_hike) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40",  "black", "grey60",  RIG.blue, RIG.red, RIG.green)) + 
	scale_shape_manual(values = c(1,16, 15, 21, 18, 19, 20)) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, 
			 y = "Probability (%)",
			 color = NULL,
			 shape = NULL) + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	theme(legend.position = c(0.01,0.99),
				legend.justification = c(0,1),
				legend.background = element_rect(fill="white", 
																				 size=0.5, linetype="solid", color = "grey70") ) + 
	RIG.theme()
fig_ERChike.fPolicy


# Figure: Risk of high ERC (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_ERChigh.fPolicy <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 7, 8, 11)]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(1, 7, 8, 11)], labels = runs_fPolicy_labels1[c(1, 7, 8, 11)])) %>% 
	select(runname.fct, year, ERC_high) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,50)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey40", "grey70", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 21, 18, 19, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_ERChigh.fPolicy




#*****************************************************
## Analysis 2 Impact of TDA in alt return scenarios ####
#*****************************************************

# Illustration of single runs
# Actual returns and effective returns: MA based
# Actual returns and effective returns: AA based
# Funded ratio
# ERC

geoR <- 
	results_all %>%
	filter(runname == "multiTier_TDAamortAS") %>% 
	filter(sim >= 1, year >= 2019) %>% 
	group_by(sim) %>% 
	summarise(geoR_noTDA = get_geoReturn(i.r),
						geoR_TDA   = get_geoReturn(i.r.wTDA)) %>% 
	arrange(geoR_noTDA)

geoR %>% filter(is.nan(geoR_TDA))



# Negative QPP asset : 1797, 1986
x <- results_all %>%
	filter(runname == "multiTier_TDAamortAS", sim == 1797) %>% select(year, i.r, i.r.wTDA,  MA, MA.TDA, MA.TDA_QPP, FR_MA, C, B)

x$i.r.wTDA %>% get_geoReturn()

geoR %>% filter(geoR_noTDA > 0.0695, geoR_noTDA <= 0.0705) 

# Volatility drag due to TDA
geoR %>% 
	summarise(geoR_noTDA = median(geoR_noTDA, na.rm = T),
						geoR_TDA   = median(geoR_TDA, na.rm = T))


# 23   912 0.06993192 0.06830289

df_singleRuns <- 
	results_all %>% 
	filter(runname %in% runs_TDA_OYLM[1:2], sim == 476) %>% 
	select(runname, year, FR_MA, ERC_PR, i.r, i.r.wTDA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_TDA_OYLM[1:2], labels = runs_TDA_OYLM_labels[1:2])) 


# Actual return vs effective return with TDA

fig_TDA_compareReturn <- 
df_singleRuns %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM") %>%
	select(runname.fct, year, i.r, i.r.wTDA) %>% 
	gather(Var, value, -year,-runname.fct) %>% 
	mutate(Var = factor(Var, levels = c("i.r", "i.r.wTDA"),
											labels = c("Actual market return",
																 "Effective return with TDA"))) %>% 
	ggplot(aes(x = year, y = value*100, color = Var)) + theme_bw()+
	geom_line() + 
	geom_point() +
	geom_hline(yintercept = 7, linetype = 2)+
	scale_y_continuous(breaks = c(7, seq(-100,100, 10)) ) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	labs(title =    "Actual returns and effective returns with TDA transfers",
			 subtitle = "sim #424; 30-year geometric return = 7.0%",
			 color = NULL,
			 x = NULL, y = "Rate of return (%)") +
	RIG.theme()
fig_TDA_compareReturn


fig_TDA_compareFR <- 
df_singleRuns %>% 
	ggplot(aes(x = year, y = FR_MA, color = runname.fct)) + theme_bw()+
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,150)) + 
	scale_y_continuous(breaks = seq(0,200, 20)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	labs(title =    "Funded ratio with different treatment of TDA",
			 subtitle = "sim #424; 30-year geometric return = 7.0%",
			 color = NULL,
			 x = NULL, 
			 y = "Funded ratio (%)") +
	RIG.theme()
fig_TDA_compareFR 


fig_TDA_compareERC <- 
df_singleRuns %>% 
	ggplot(aes(x = year, y = ERC_PR, color = runname.fct)) + theme_bw()+
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,50)) + 
	scale_y_continuous(breaks = seq(-100,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	labs(title =    "Employer contribution rate with different treatment of TDA",
			 subtitle = "sim #424; 30-year geometric return = 7.0%",
			 color = NULL,
			 x = NULL, y = "Employer contribution Rate (%)") +
	RIG.theme()
fig_TDA_compareERC 


save_figure(fig_TDA_compareReturn,  width = 2*5, height = 1*5)
save_figure(fig_TDA_compareFR,      width = 2*5, height = 1*5)
save_figure(fig_TDA_compareERC,     width = 2*5, height = 1*5)




runs_RS1 <-   c("multiTier_noTDA_OYLM",
								"multiTier_TDAamortAS_OYLM",
								"multiTier_noTDA_OYLM_low15",
								"multiTier_TDAamortAS_OYLM_low15"
								#"multiTier_noTDA_OYLM_low30",
								#"multiTier_TDAamortAS_OYLM_low30"
								)

runs_RS1_label <- c("t4a_noTDA_OYLM",
									 "t4a_TDAamortAS_OYLM",
									 
									 "15 years of low returns; w/o TDA",
									 "15 years of low returns; w/ TDA"
									 #"30 years of low returns; w/o TDA",
									 #"30 years of low returns; w/ TDA"
										)


df_all.stch %>% 
	filter(runname %in% runs_RS1, year %in% c(2048), str_detect(runname, "noTDA_OYLM")) 

df_all.stch %>% 
	filter(runname %in% runs_RS1, year %in% c(2048), str_detect(runname, "TDAamortAS_OYLM")) 



# Tables of key risk measures
df_all.stch %>% 
	filter(runname %in% runs_RS1, year == 2046) %>% 
	arrange(year)

df_all.stch$runname


# Figure: Risk of low funded ratio (FR40less and FR50less, 3 lines in each graph) 4 by 4

fig.title <- "Probability of funded ratio below 40% or 50% in any year up to the given year\nunder alternative return scenarios"
fig.subtitle <- NULL#"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_TDA_FR40less_4fig <- df_all.stch %>% 
	filter(runname %in% runs_RS1) %>% 
	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
	select(runname.fct, TDA_policy, return_scenario, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	gather(variable, value, -year, -runname.fct, -TDA_policy, -return_scenario) %>% 
	mutate(variable = factor(variable, 
													 levels = c("FR40less", "FR50less"),
													 labels = c("Funded ratio below 40%", "Funded ratio below 50%")),
				 return_scenario = factor(return_scenario, 
				 								   	     levels = c("RS1", "RS_15low"),
				 									       labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
				 TDA_policy = factor(TDA_policy, 
				 												 levels = c("noTDA", "TDAamortAS"),
				 												 labels = c("Without TDA", "With TDA"))
				 
				 
				 ) %>% 
	ggplot(aes(x = year, y = value, color =TDA_policy, shape = TDA_policy)) + theme_bw() + 
	facet_grid(variable~ return_scenario) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,60)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16,15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_TDA_FR40less_4fig 


fig.title <- "Probability of funded ratio below 40% in any year up to the given year\nunder alternative return scenarios"
fig.subtitle <- NULL#"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_TDA_FR40less_2fig <- df_all.stch %>% 
	filter(runname %in% runs_RS1) %>% 
	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
	select(runname.fct, TDA_policy, return_scenario, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	# gather(variable, value, -year, -runname.fct, -TDA_policy, -return_scenario) %>% 
	mutate(
		#      variable = factor(variable, 
		# 											 levels = c("FR40less", "FR50less"),
		# 											 labels = c("Funded ratio below 40%", "Funded ratio below 50%")),
				 return_scenario = factor(return_scenario, 
				 												 levels = c("RS1", "RS_15low"),
				 												 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
				 TDA_policy = factor(TDA_policy, 
				 										levels = c("noTDA", "TDAamortAS"),
				 										labels = c("Without TDA", "With TDA"))
				 
				 
	) %>% 
	ggplot(aes(x = year, y = FR40less, color =TDA_policy, shape = TDA_policy)) + theme_bw() + 
	facet_grid(.~ return_scenario) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,60)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16,15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_TDA_FR40less_2fig




# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year\nunder alternative return scenarios"
fig.subtitle <- NULL# "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_TDA_ERChike <- df_all.stch %>% 
	filter(runname %in% runs_RS1) %>% 
	select(runname.fct, TDA_policy, return_scenario, year, ERC_hike) %>% 
	mutate(
				 return_scenario = factor(return_scenario, 
				 												 levels = c("RS1", "RS_15low"),
				 												 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
				 TDA_policy = factor(TDA_policy, 
				 										levels = c("noTDA", "TDAamortAS"),
				 										labels = c("Without TDA", "With TDA"))) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike, color = TDA_policy, shape = TDA_policy)) + theme_bw() + 
	facet_grid(.~return_scenario) + 
	geom_point(size = 2) + geom_line() +  
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_TDA_ERChike


# Figure: Risk of high ERC (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time prior to and including the given year\nunder alternative return scenarios"
fig.subtitle <- NULL #"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_TDA_ERChigh <- df_all.stch %>% 
	filter(runname %in% runs_RS1) %>% 
	select(runname.fct, year, TDA_policy, return_scenario, ERC_high) %>% 
	mutate(
		return_scenario = factor(return_scenario, 
														 levels = c("RS1", "RS_15low"),
														 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
		TDA_policy = factor(TDA_policy, 
												levels = c("noTDA", "TDAamortAS"),
												labels = c("Without TDA", "With TDA"))) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high, color = TDA_policy, shape = TDA_policy )) + theme_bw() + 
	facet_grid(.~return_scenario) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_TDA_ERChigh



fig.title    <- "Distribution of employer contribution rates without TDA transfers across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7% (w/o TDA transfer)"
fig_TDA_ERCdist <-
	df_all.stch %>%
	filter(runname %in% runs_TDA_OYLM[1:2]) %>%
	select(runname.fct, year,
				 ERC_PR.q25,
				 ERC_PR.q50,
				 ERC_PR.q75,
				 ERC_PR.q90) %>%
	gather(Var, value, -runname.fct, -year) %>%
	# mutate(
	# 	runname = factor(runname, levels = runs_test, labels = runs_test_labels)
	# ) %>%
	#mutate(type = ifelse(str_detect(Var, "TDA"), "wTDA", "noTDA"),
	#       Var  = str_replace(Var, "TDApayouts_", "")) %>%
	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%
	ggplot(aes(x = year, y = value,
						 color = factor(Var, levels = c("ERC_PR.q90", "ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) +
	facet_grid(. ~ runname.fct) +
	theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	coord_cartesian(ylim = c(-20,100)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(-100, 100, 10)) +
	scale_color_manual(values = c("red", RIG.red, RIG.blue, RIG.green),  name = NULL,
										 label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile")) +
	scale_shape_manual(values = c(14, 17, 16, 15, 18),  name = NULL,
										 label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile")) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "%") +
	theme(axis.text.x = element_text(size = 8)) + 
	RIG.theme()
fig_TDA_ERCdist


# Figure: Distribution of funded ratio

fig.title    <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7% (w/o TDA transfer)"
fig_TDA_FRdist <- df_all.stch %>%
	filter(runname %in% runs_TDA_OYLM[1:2]) %>%
	select(runname.fct, year, FR.q75, FR.q50, FR.q25, FR.q10) %>%
	gather(type, value, -runname.fct, -year) %>%
	mutate(type = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR.q10"),
											 labels = c("75th percentile", "50th percentile", "25th percentile", "10th percentile")
	)) %>%
	ggplot(aes(x = year,
						 y = value,
						 color = type),
				 shape = type) + theme_bw() +
	facet_grid(.~runname.fct) +
	geom_line() +
	geom_point(size = 2) +
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,170)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) +
	scale_y_continuous(breaks = seq(0, 500, 20)) +
	scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Percent") +
	theme(axis.text.x = element_text(size = 8)) +
	RIG.theme()

fig_TDA_FRdist


save_figure(fig_TDA_FR40less_4fig, w = 2*4, h = 1.6*4 )
save_figure(fig_TDA_FR40less_2fig, w = 2*5, h = 1*5 )
save_figure(fig_TDA_ERChike,       w = 2*5, h = 1*5 )
save_figure(fig_TDA_ERChigh,       w = 2*5, h = 1*5 )
save_figure(fig_TDA_FRdist,        w = 2*5, h = 1*5 )




#*****************************************************
## Analysis 3 Asset schock scenarios              ####
#*****************************************************


results_all %>% 
	filter(runname %in% runs_DF, sim == 1, year %in% c(2019, 2020, 2021, 2048)) %>% 
	select(runname, year, FR_MA, ERC_PR, AA, MA, ERC_PR) 
	
fig.title <- "TRS funded ratio under hypothetical asset shock scenario"
fig.subtitle <- NULL

fig_shock1_FR <- 
results_all %>% 
	filter(runname %in% runs_DF[1:2], sim == 1) %>% 
	select(runname, year, FR_MA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[1:2], labels = runs_DF_labels[1:2])) %>% 
	ggplot(aes(x = year,
						 y = FR_MA,
						 color = runname.fct,
				     shape = runname.fct)) + theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,100)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 500, 20)) +
	scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Funded ratio (%)") +
	theme(axis.text.x = element_text(size = 8),
				legend.position = "bottom",
				legend.key.width = unit(0.5, "inch" )) +
	RIG.theme()
fig_shock1_FR


fig.title <- "TRS employer contribution rate under hypothetical asset shock scenario"
fig.subtitle <- NULL
fig_shock1_ERC <- 
	results_all %>% 
	filter(runname %in% runs_DF[1:2], sim == 1) %>% 
	select(runname, year, ERC_PR) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[1:2], labels = runs_DF_labels[1:2])) %>% 
	ggplot(aes(x = year,
						 y = ERC_PR,
						 color = runname.fct,
						 shape = runname.fct)) + theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	coord_cartesian(ylim = c(0,60)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 500, 10)) +
	scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Employer contribution as a percentage of payroll") +
	theme(axis.text.x = element_text(size = 8),
				legend.position = "bottom",
				legend.key.width = unit(0.5, "inch" )) +
	RIG.theme()
fig_shock1_ERC


save_figure(fig_shock1_ERC, w = 1.6*5, h = 1*5 )
save_figure(fig_shock1_FR,  w = 1.6*5, h = 1*5 )







fig.title <- "TRS funded ratio under hypothetical asset shock scenario"
fig.subtitle <- NULL
fig_shock2_FR <- 
	results_all %>% 
	filter(runname %in% runs_DF[3:4], sim == 1) %>% 
	select(runname, year, FR_MA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[3:4], labels = runs_DF_labels[3:4])) %>% 
	ggplot(aes(x = year,
						 y = FR_MA,
						 color = runname.fct,
						 shape = runname.fct)) + theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,100)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 500, 20)) +
	scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Funded ratio (%)") +
	theme(axis.text.x = element_text(size = 8)) +
	RIG.theme()
fig_shock2_FR



fig.title <- "TRS employer contribution rate under hypothetical asset shock scenario"
fig.subtitle <- NULL
fig_shock2_ERC <- 
results_all %>% 
	filter(runname %in% runs_DF[3:4], sim == 1) %>% 
	select(runname, year, ERC_PR) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[3:4], labels = runs_DF_labels[3:4])) %>% 
	ggplot(aes(x = year,
						 y = ERC_PR,
						 color = runname.fct,
						 shape = runname.fct)) + theme_bw() +
	geom_line() +
	geom_point(size = 2) +
	coord_cartesian(ylim = c(0,60)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 500, 10)) +
	scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Employer contribution as a percentage of payroll") +
	theme(axis.text.x = element_text(size = 8)) +
	RIG.theme()
fig_shock2_ERC


# try ggfan


# generate mean and variance for sequence of samples over time
N_time <- 50
N_sims <- 1000 
time <- 1:N_time
mu <- time**2 * 0.03 + time * 0.3
sds <- exp(time**2 * -0.001 + time * 0.1)

# simulate 1000 samples from each time point
fake_data <- sapply(time, function(i) rnorm(N_sims, mu[i], sds[i]))

# gather into a long-form, tidy dataset
fake_df <- data.frame(x=time, t(fake_data)) %>% gather(key=Sim, value=y, -x)

head(fake_df)

fake_q1 <-  fake_df %>% calc_quantiles(intervals=c(0,0.5, 0.8)) 

names(fake_q1)

fake_q1 %>% head

ggplot(fake_q1,  aes(x=x,y=y,quantile=quantile)) + 
	geom_fan(intervals = c(0.2,0.5, 0.8))

intervals = 1:19/20
fake_q <- calc_quantiles(fake_df, intervals=intervals)
fake_q
# intervals in geom_fan must be the same as used to compute quantiles.
ggplot(fake_q, aes(x=x,y=y, quantile=quantile)) +
	geom_fan(intervals=intervals)




df_all.stch %>%
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019) %>%
	select(runname.fct, year,
				 ERC_PR.q10,
				 ERC_PR.q25,
				 ERC_PR.q50,
				 ERC_PR.q75,
				 ERC_PR.q90) %>%
	gather(Var, value, -runname.fct, -year) %>% 
	mutate(Var_q = as.numeric(str_extract(Var, "\\d+"))/100) %>% 
	ggplot(aes ( x= year, y = value, quantile = Var_q)) + 
	geom_fan() + 
	geom_line(aes(x = year, y = value, linetype = Var))+
	geom_point(aes(x = year, y = value, shape = Var))+
	scale_fill_gradient(low="lightblue3", high="lightblue1")




results_all %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019, sim > 0) %>%
  select(year, sim, ERC_PR) %>% 
	ggplot(aes(x=year,y=ERC_PR)) + geom_fan() +
	scale_fill_gradient(low="lightblue4", high="lightblue1")



df_q <- 
df_all.stch %>%
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019) %>%
	select(runname.fct, year,
				 FR.q10,
				 FR.q25,
				 FR.q50,
				 FR.q75,
				 FR.q90) %>%
	gather(Var, value, -runname.fct, -year) %>% 
	mutate(Var_q = as.numeric(str_extract(Var, "\\d+"))/100)



results_all %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >= 2019, sim > 0) %>%
	select(year, sim, FR_MA) %>% 
	ggplot(aes(x=year,y=FR_MA)) + 
	geom_fan(intervals = 1:80/100) + 
	geom_line(data = df_q,  mapping  = aes(x = year, y = value, linetype = Var))+
	geom_point(data = df_q, mapping = aes(x = year, y = value,  shape    = Var), size = 2 , color = RIG.red)+
	scale_fill_gradient(low="red", high="pink") +
  scale_linetype_manual(values = c(2, 2, 2, 2, 2 ) ) +
	scale_shape_manual(values = c(17,16, 15, 21, 18, 19, 20)) +
	labs(linetype = NULL,
			 shape = NULL)+
	guides(fill = FALSE)
	



# ggplot template


# fig.title    <- "Investment returns in three single simulation runs "
# fig.subtitle <- " All three runs have the same 30-year geometric return of 7.0%"
# # fig_singleReturns1 <- 
# # 	df_singleRuns1 %>% 
# # 	filter(year >= 2018) %>% 
# # 	select(runname.fct, sim, year, i.r) %>% 
# # 	#gather(Var, value, -year,-runname.fct) %>% 
# # 	mutate(sim = factor(sim, levels = singleRuns1, 
# # 											labels = c("Constant return", paste0("Sim ", singleRuns1[2]), paste0("Sim ", singleRuns1[3])))) %>%
# 	ggplot(aes(x = year, y = i.r*100, color = sim)) + theme_bw()+
# 	geom_line() + 
# 	geom_point() +
# 	geom_hline(yintercept = 7, linetype = 2)+
# 	scale_y_continuous(breaks = c(7, seq(-100,100, 5)) ) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
# 	scale_color_manual(values = c("black", "deepskyblue1", "firebrick1")) + 
# 	labs(title = fig.title   ,
# 			 subtitle = fig.subtitle,
# 			 color = NULL,
# 			 x = NULL, 
# 			 y = "Rate of return (%)") +
# 	RIG.theme() +
# 	theme(legend.position = "bottom")
# 
# fig_singleReturns1






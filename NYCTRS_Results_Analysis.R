# Risk measures for NYCTRS

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
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

source("Functions.R")



#*****************************************************
##  Notes         ####
#*****************************************************

# Analysis 1: Impact of TDA

# Three scenarios
#  - no TDA
#  - TDA payments are amortized
#  - TDA payments are not amortized


# Analysis 2: Impact of funcing policy 

# Three scenarios
#  - Plan policy (TDA payments are amortized)
#  - More backloaded amortization policies: cd/cp, open/closed, 15->30
#  - One-Year-Lag-Method






#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
# Outputs_folder  <- "Results/Graphs_report/"



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





#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

runs_TDA <-   c("t4a_noTDA",
								"t4a_TDAamort",
								"t4a_TDApayout")

runs_TDA_labels <- c("Without TDA",
										 "TDA amortized",
										 "TDA not amortized")


runs_fPolicy <-   c(
										"t4a_O15dA6",
										"t4a_O15pA6",
										"t4a_O30pA6",
										"t4a_C30dA6",
										"t4a_C15pA6",
										"t4a_C15dA0"
								    )

runs_fPolicy_labels <- c("Open amort.",
												 "Open level pct amort.",
												 "open level pct 30-year amort.",
	                       "30-year amort.",
												 "Level percent amort.",
												 "no asset smoothing")


runs_all <- c(runs_TDA, runs_fPolicy)
runs_all_labels <- c(runs_TDA_labels, runs_fPolicy_labels)



# Calculate total final ERC rate for runs with DC reform (include ERC to DC in ERC.final_PR)


# in the past 5 years ERC rate
#ERC_rate_5y <- data.frame(year = 2012:2016, ERC_rate_5y = c(0.0869,0.0915, 0.0915 ,0.0998, 0.1078))
# ERC_rate_5y <- c(8.69,9.15, 9.15 ,9.98, 10.78)




#*****************************************************
##  Computing risk measures ####
#*****************************************************

df_all.stch <- 
  results_all %>%
	filter(runname %in% runs_all) %>% 
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
				 
				 
	) %>%
	group_by(runname, year) %>%
	summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
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
						
						ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
						ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
						ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
						ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
						ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)

	) %>% 
	mutate(runname.fct = factor(runname, levels = runs_all, labels = runs_all_labels)) %>%
	arrange(runname.fct) %>% 
	ungroup() 

	
	
df_all.stch %>% 
	filter(year %in% c(2016, seq(2020, 2045, 5)))


df_all.stch %>% 
  filter(year %in% 2045)



results_all %>% 
	select(runname, sim, year, AL, MA, AA, FR, SC, ERC,EEC,PR, Amort_basis) %>% 
	filter(sim == 912, year %in% c(2016:2045), runname %in% "t4a_TDAamort")

x <- 
results_all %>% filter(runname == "t4a_TDAamort") %>% 
	group_by(sim) %>% 
	summarise(SD = sd(i.r.wTDA),
						SD.r = sd(i.r)) %>% 
	summarise(SD.med = median(SD),
						SD.r.med = median(SD.r))

x

x$i.r.wTDA %>% sd

x$i.r %>% sd

#*****************************************************
## Compare key results  ####
#*****************************************************

# Risk of Low funded ratio: 
# Under immediate recognition and payment of TDA interests, risk measures are identical across
# all test scenarios. 

results_all %>% 
  select(runname, sim, year, AL, NC_PR, ERC_PR, PVFB, B, PR) %>% 
  mutate(runname = factor(runname, levels = runs_test, labels = runs_test_labels)) %>%
  filter(sim == 0, year %in% c(2016))


df_all.stch %>% 
  select(runname, year, FR40less) %>% 
  mutate(runname = factor(runname, levels = runs_test, labels = runs_test_labels)) %>% 
  spread(runname, FR40less) %>% 
  filter(year %in% c(2016, 2020, 2030, 2040, 2045))


df_all.stch %>% 
  select(runname, year, ERC_hike) %>% 
  mutate(runname = factor(runname, levels = runs_test, labels = runs_test_labels)) %>% 
  spread(runname, ERC_hike) %>% 
  filter(year %in% c(2016, 2020, 2030, 2040, 2045))


df_all.stch %>% 
  select(runname, year, ERC_TDA_hike) %>% 
  mutate(runname = factor(runname, levels = runs_test, labels = runs_test_labels)) %>% 
  spread(runname, ERC_TDA_hike) %>% 
  filter(year %in% c(2016, 2020, 2025, 2030, 2040, 2045))


df_all.stch %>% 
  select(runname, year, ERC_high) %>% 
  mutate(runname = factor(runname, levels = runs_test, labels = runs_test_labels)) %>% 
  spread(runname, ERC_high) %>% 
  filter(year %in% c(2016, 2020, 2030, 2040, 2045))


df_all.stch %>% 
  select(runname, year, ERC_TDA_high) %>% 
  mutate(runname = factor(runname, levels = runs_test, labels = runs_test_labels)) %>% 
  spread(runname, ERC_TDA_high) %>% 
  filter(year %in% c(2016, 2020, 2030, 2040, 2045))



#*****************************************************
## Analysis 1 Impact of TDA ####
#*****************************************************


# Illustration of single runs
 # Actual returns and effective returns: MA based
 # Actual returns and effective returns: AA based
 # Funded ratio
 # ERC



geoR <- 
	results_all %>%
	filter(runname == "t4a_TDAamort") %>% 
	filter(sim >= 1, year >= 2019) %>% 
	group_by(sim) %>% 
	summarise(geoR_noTDA = get_geoReturn(i.r),
						geoR_TDA   = get_geoReturn(i.r.wTDA)) %>% 
	arrange(geoR_noTDA)





geoR %>% filter(geoR_noTDA > 0.0695, geoR_noTDA <= 0.0705)

# Volatility drag due to TDA
geoR %>% 
	summarise(geoR_noTDA = median(geoR_noTDA),
						geoR_TDA   = median(geoR_TDA))


# 23   912 0.06993192 0.06830289

df_singleRuns <- 
results_all %>% 
	filter(runname %in% runs_TDA, sim == 912) %>% 
	select(runname, year, FR_MA, ERC_PR, i.r, i.r.wTDA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_all, labels = runs_all_labels)) 

df_singleRuns %>% 
	filter(runname %in% "t4a_TDAamort") %>%
	select(runname.fct, year, i.r, i.r.wTDA) %>% 
	gather(Var, value, -year,-runname.fct) %>% 
	mutate(Var = factor(Var, levels = c("i.r", "i.r.wTDA"),
											     labels = c("Actual market return",
											     					   "Effective return with TDA"))) %>% 
	ggplot(aes(x = year, y = value*100, color = Var)) + theme_bw()+
	geom_line() + 
	geom_point() +
	scale_y_continuous(breaks = seq(-100,100, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	labs(title =    "Actual returns and effective returns with TDA transfers",
			 subtitle = "sim #912; 30-year geometric return = 7.0%",
			 color = NULL,
			 x = NULL, y = "Rate of return (%)") +
	RIG.theme()


df_singleRuns %>% 
	ggplot(aes(x = year, y = FR_MA, color = runname.fct)) + theme_bw()+
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,150)) + 
	scale_y_continuous(breaks = seq(0,200, 20)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	labs(title =    "Funded ratio with different treatment of TDA",
			 subtitle = "sim #912; 30-year geometric return = 7.0%",
			 color = NULL,
			 x = NULL, 
			 y = "Funded ratio (%)") +
	RIG.theme()
	

df_singleRuns %>% 
	ggplot(aes(x = year, y = ERC_PR, color = runname.fct)) + theme_bw()+
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,80)) + 
	scale_y_continuous(breaks = seq(-100,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	labs(title =    "Employer contribution rate with different treatment of TDA",
			 subtitle = "sim #912; 30-year geometric return = 7.0%",
			 color = NULL,
			 x = NULL, y = "Employer contribution Rate (%)") +
	RIG.theme()






# Tables of key risk measures
df_all.stch %>% 
	filter(runname %in% runs_TDA, year %in% c(2030, 2045)) %>% 
	arrange(year)


# Figure: Risk of low funded ratio (FR40less and FR50less, 3 lines in each graph)

fig.title <- "Probability of funded ratio below 40% or 50% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_FR40less <- df_all.stch %>% 
	filter(runname %in% runs_TDA) %>% 
	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
	select(runname.fct, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	gather(variable, value, -year, -runname.fct) %>% 
	mutate(variable = factor(variable, 
													 levels = c("FR40less", "FR50less"),
													 labels = c("Funded ratio below 40%", "Funded ratio below 50%"))) %>% 
	ggplot(aes(x = year, y = value, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	facet_grid(.~ variable) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,40)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16,15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_FR40less



# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_ERChike <- df_all.stch %>% 
	filter(runname %in% runs_TDA) %>% 
	select(runname.fct, year, ERC_hike) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_ERChike


# Figure: Risk of high ERC (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_ERChigh <- df_all.stch %>% 
	filter(runname %in% runs_TDA) %>% 
	select(runname.fct, year, ERC_high) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	scale_shape_manual(values = c(17,16, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_ERChigh


# Figure: Distribution of FR 

fig.title    <- "Distribution of employer contribution rates without TDA transfers across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7% (w/o TDA transfer)"
fig_ERCdist <- 
	df_all.stch %>% 
	filter(runname %in% runs_TDA) %>% 
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
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
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
fig_ERCdist


# Figure: Dsitribution of ERC

fig.title    <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7% (w/o TDA transfer)"
fig_FRdist <- df_all.stch %>%  
	filter(runname %in% runs_TDA) %>% 
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
	coord_cartesian(ylim = c(0,160)) + 
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 20)) + 
	scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "red"),  name = NULL) + 
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Percent") + 
	theme(axis.text.x = element_text(size = 8)) + 
	RIG.theme()

fig_FRdist


#*****************************************************
## Analysis 2 Impact of Funding policy ####
#*****************************************************


runs_fPolicy1 <-   c(
	"t4a_TDAamort",
	"t4a_C15dA0",
	"t4a_O15dA6",
	"t4a_O15pA6",
	"t4a_O30pA6",
	"t4a_C30dA6",
	"t4a_C15pA6"

)

runs_fPolicy_labels1 <- c("TRS policy",
												 "no asset smoothing",
	                       "Open amort.",
												 "Open level pct amort.",
												 "open level pct 30-year amort.",
												 "30-year amort.",
												 "Level percent amort."
												 )


fig.title <- "Probability of funded ratio below 40% or 50% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_lowFR.fPolicy <- 
df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:5]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:5], labels = runs_fPolicy_labels1[1:5])) %>% 
	select(runname.fct, year, FR40less, FR50less) %>% 
	#mutate(FR40less.det = 0) %>% 
	gather(variable, value, -year, -runname.fct) %>% 
	mutate(variable = factor(variable, 
													 levels = c("FR40less", "FR50less"),
													 labels = c("Funded ratio below 40%", "Funded ratio below 50%"))) %>% 
	ggplot(aes(x = year, y = value, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	facet_grid(.~ variable) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,60)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 18, 19),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_lowFR.fPolicy


# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_ERChike.fPolicy <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:5]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:5], labels = runs_fPolicy_labels1[1:5])) %>% 
	select(runname.fct, year, ERC_hike) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 18, 19),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_ERChike.fPolicy


# Figure: Risk of high ERC (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig_ERChigh.fPolicy <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[1:5]) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[1:5], labels = runs_fPolicy_labels1[1:5])) %>% 
	select(runname.fct, year, ERC_high) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high, color = runname.fct, shape = runname.fct)) + theme_bw() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,50)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", "grey60",  RIG.blue, RIG.red, RIG.green),  name = "") + 
	scale_shape_manual(values = c(17,16,15, 18, 19),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability (%)") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig_ERChigh.fPolicy





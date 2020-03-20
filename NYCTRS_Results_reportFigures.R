# Risk measures for NYCTRS

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20

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
# library("btools")
library("scales")
library(gridExtra)
library(grid)
library(ggplot2)
# library(plotly)
# library(ggtern) # converting rgb to hex


## Using extra fonts
# packages: 
#  # extrafonts, only support ttf fonts (Gotham is otf) 
#  # showtext

## References:
# https://stackoverflow.com/questions/52251290/add-font-to-r-that-is-not-in-extrafonts-library
# https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html

## using extrafonts:
library(extrafont)
# font_import(paths = "C:\\Users\\yimen\\AppData\\Local\\Microsoft\\Windows\\Fonts", pattern = "Gotham")
loadfonts()
# windowsFonts()
# windowsFonts(windowsFont("Gotham Book"))


## using showtext
# library(showtext)
# font_add(family = "Gotham_Book",   regular = "C:\\Users\\yimen\\AppData\\Local\\Microsoft\\Windows\\Fonts\\Gotham_Book.otf")
# font_add(family = "Gotham_Medium", regular = "C:\\Users\\yimen\\AppData\\Local\\Microsoft\\Windows\\Fonts\\Gotham_Medium_Regular.ttf")
# showtext_auto()


source("Functions.R")


{
	marron_blue1  <- "#025b74" # ggtern::rgb2hex(2,91,116)
	marron_blue2  <- "#00aaad" # ggtern::rgb2hex(0,170,173)
	marron_green  <- "#386742" # ggtern::rgb2hex(56,103,66)
	marron_brown1 <- "#544043" # ggtern::rgb2hex(84,64,67)
	marron_brown2 <- "#844349" # ggtern::rgb2hex(132, 67, 73)
	marron_red    <- "#c9252c" # ggtern::rgb2hex(201, 37, 44)
	marron_orange <- "#b95630" # ggtern::rgb2hex(185, 86, 48)
	
	
	marron_colors <- c(marron_blue1,
									   marron_blue2,
									   marron_green ,
									   marron_brown1,
									   marron_brown2,
										 marron_red,
										 marron_orange)
	
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
	

	theme_marron <- function(){
		theme_bw() +
			theme(  
			    	panel.border       = element_rect(size = 0.5, color = "gray50"),
				    panel.grid.major.x = element_line(size = 0.4, color = "gray90"), #element_blank(),
						panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_blank(),
						
						plot.caption=element_text(hjust=0, size = 9),
						
						text = element_text(family = "Gotham Book", color = "black", size = 8),
						plot.title    = element_text(family = "Gotham Medium", size = 10, hjust=0.5),
						plot.subtitle = element_text(family = "Gotham Book",   size = 9, hjust=0.5),
						legend.text   = element_text(family = "Gotham Book", size = 7)
			)
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
Outputs_folder  <- "Outputs_figures/Figures_FinalReport/"


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
# results_all %>% head



#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

runs_TDA <-   c("multiTier_noTDA",
								"multiTier_TDAamortAS",
								"multiTier_TDApayout")

runs_TDA_OYLM <- c("multiTier_noTDA_OYLM",
									 "multiTier_TDAamortAS_OYLM",
								   "multiTier_TDApayout_OYLM")

runs_TDA_alt <- c("multiTier_TDAamortAS_OYLM_TDA_highG",
                  "multiTier_TDAamortAS_OYLM_TDA_lowG",
									
									"multiTier_TDAamortAS_OYLM_TDA_LowRate1_base",
                  "multiTier_TDAamortAS_OYLM_TDA_LowRate1_lowG",
									
									"multiTier_TDAamortAS_OYLM_TDA_LowRate2_base",
									"multiTier_TDAamortAS_OYLM_TDA_LowRate2_lowG")


runs_TDA_labels <- c("Without TDA",
										 "TDA amortized; AS",
										 "TDA not amortized")

runs_TDA_OYLM_labels <- c("Without TDA; OYLM",
										      "TDA amortized; AS; OYLM",
										      "TDA not amortized; OYLM"
													)


runs_TDA_alt_labels <- 
	              c("Fast growth of TDA",
									"Slow growth of TDA",
									
									"Lower TDA fixed return: 5% \nBaseline Growth",
									"Lower TDA fixed return: 5% \nSlower Growth",
									
									"Lower TDA fixed return: 2.5% \nBaseline Growth",
									"Lower TDA fixed return: 2.5% \nSlower Growth"
									)

runs_TDA_offset <- c("multiTier_TDAamortAS_OYLM_offset10",
										 "multiTier_TDAamortAS_OYLM_offset15",
										 "multiTier_TDAamortAS_OYLM_offset18")

runs_TDA_offset_lables <- c("Addtional ERC of 10% of payroll",
										        "Addtional ERC of 15% of payroll",
														"Addtional ERC of 18% of payroll")


runs_RS <- c("multiTier_noTDA_OYLM_low15",
						 "multiTier_TDAamortAS_OYLM_low15",
						 "multiTier_noTDA_OYLM_low30",
						 "multiTier_TDAamortAS_OYLM_low30"
						 )

runs_RS_labels <- c("15 years of low returns; no TDA",
										"15 years of low returns; with TDA",
										"30 years of low returns; no TDA",
										"30 years of low returns; with TDA"
)


runs_DF <- c("multiTier_noTDA_OYLM_DF1",
						 "multiTier_TDAamortAS_OYLM_DF1",
						 "multiTier_noTDA_OYLM_DF2",
						 "multiTier_TDAamortAS_OYLM_DF2"
)

runs_DF_labels <- c("Asset shock scenario; no TDA",
										"Asset shock scenario; with TDA",
										"Asset shock scenario 2; no TDA",
										"Asset shock scenario 2; with TDA"
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


runs_all        <- c(runs_TDA, runs_TDA_OYLM, runs_TDA_alt, runs_RS, runs_DF, runs_fPolicy, runs_TDA_offset)
runs_all_labels <- c(runs_TDA_labels, runs_TDA_OYLM_labels, runs_TDA_alt_labels, runs_RS_labels, runs_DF_labels, runs_fPolicy_labels,runs_TDA_offset)



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
	mutate(NC_wtermCost  = ifelse(str_detect(runname, "noTDA"), NC, NC  + termCost_UFT),
				 ERC_wtermCost = ifelse(str_detect(runname, "noTDA"), ERC, ERC + termCost_UFT),
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

# showtext_begin()

fig_dpi = 500
w_singleFig <- 2*3.7
h_singleFig <- 1.3*3.7


runs_fPolicy_labels1 <- c("TRS policy",
												 "No One-Year-Lag-Method",
												 "No asset smoothing",
												 "No corridor",
	                       "Open amort.",
												 "Open level percent amort.",
												 "Open level percent\n30-year amort.",
												 "Open level percent\n30-year amort; noTDA",
												 "30-year amort.",
												 "Level percent amort.",
												 "No TDA"
												 )


## Figure 4 ####
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
fig4_policy_lowFR_select <- 
	df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 4, 7)], year >=2019) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(1, 4, 7)], labels = runs_fPolicy_labels1[c(1, 4, 7)])) %>% 
	select(runname.fct, year, FR40less) %>% 
	#mutate(FR40less.det = 0) %>% 
	# gather(variable, value, -year, -runname.fct) %>% 
	# mutate(variable = factor(variable, 
	# 												 levels = c("FR40less", "FR50less"),
	# 												 labels = c("Funded ratio below 40%", "Funded ratio below 50%"))) %>% 
	ggplot(aes(x = year, y = FR40less/100, color = runname.fct, shape = runname.fct)) + theme_marron() + # theme_bw() + 
	# facet_grid(.~ variable) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,0.65)) + 
	scale_y_continuous(breaks = seq(0,2, 0.1), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black", marron_colors),  name = "") + 
	scale_shape_manual(values = c(17, 21, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) 
fig4_policy_lowFR_select


# # Figure: Risk of ERC hike (3 lines in a single graph)
# 
# fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time up to the given year"
# fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
# fig_policy_ERChike_select <- df_all.stch %>% 
# 	filter(runname %in% runs_fPolicy1[c(1, 4, 7)], year >=2018) %>% 
# 	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(1, 4, 7)], labels = runs_fPolicy_labels1[c(1, 4, 7)])) %>% 
# 	select(runname.fct, year, ERC_hike) %>% 
# 	#mutate(ERChike.det = 0) %>% 
# 	# gather(type, value, -year, -runname) %>% 
# 	ggplot(aes(x = year, y = ERC_hike/100, color = runname.fct, shape = runname.fct)) + theme_marron() + 
# 	geom_point(size = 2) + geom_line() + 
# 	coord_cartesian(ylim = c(0,1)) + 
# 	scale_y_continuous(breaks = seq(0,2, 0.1), labels = percent) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
# 	scale_color_manual(values = c("black", marron_colors),  name = "") + 
# 	scale_shape_manual(values = c(17, 21, 20),  name = "") +
# 	labs(title = fig.title,
# 			 subtitle = fig.subtitle,
# 			 x = NULL, y = "Probability") + 
# 	guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) 
# fig_policy_ERChike_select


# Figure 5 ####
# Figure: Risk of high ERC (3 lines in a single graph)
fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
fig5_policy_ERChigh_select <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 4, 7)], year >= 2019) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(1, 4, 7)], labels = runs_fPolicy_labels1[c(1, 4, 7)])) %>% 
	select(runname.fct, year, ERC_high) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high/100, color = runname.fct, shape = runname.fct)) + theme_marron() + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,0.3)) + 
	scale_y_continuous(breaks = seq(0,2, 0.05), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
	scale_color_manual(values = c("black", marron_colors),  name = "") + 
	scale_shape_manual(values = c(17, 21, 20),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	RIG.theme()
fig5_policy_ERChigh_select


save_figure(fig4_policy_lowFR_select,  w = w_singleFig, h = h_singleFig, units = "in", dpi = fig_dpi)
save_figure(fig5_policy_ERChigh_select,w = w_singleFig, h = h_singleFig, units = "in", dpi = fig_dpi)




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


df_singleRuns <- 
	results_all %>% 
	filter(runname %in% runs_TDA_OYLM[1:2], sim == 476) %>% 
	select(runname, year, FR_MA, ERC_PR, i.r, i.r.wTDA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_TDA_OYLM[1:2], labels = runs_TDA_OYLM_labels[1:2])) 


# Actual return vs effective return with TDA
# Figure 8 ####
fig8_TDA_compareReturn <- 
df_singleRuns %>% 
	filter(runname %in% "multiTier_TDAamortAS_OYLM", year >=2019) %>%
	select(runname.fct, year, i.r, i.r.wTDA) %>% 
	gather(Var, value, -year,-runname.fct) %>% 
	mutate(Var = factor(Var, levels = c("i.r", "i.r.wTDA"),
											labels = c("Actual market return",
																 "Effective return with TDA"))) %>% 
	ggplot(aes(x = year, y = value, color = Var, shape = Var)) + theme_marron() +
	geom_line() + 
	geom_point() +
	geom_hline(yintercept = 0.07, linetype = 2)+
	scale_y_continuous(breaks = c(0.07, seq(-100,100, 0.1)), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	# scale_color_manual(values = c(marron_blue2, marron_orange)) + 
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title =    "Actual returns and effective returns with TDA transfers",
			 subtitle = "sim #424; 30-year geometric return = 7.0%",
			 color = NULL,
			 shape = NULL,
			 x = NULL, y = "Rate of return") + 
	theme(legend.position = "bottom" )

fig8_TDA_compareReturn


# fig_TDA_compareFR <- 
# df_singleRuns %>% 
# 	filter(year >= 2018) %>% 
# 	mutate(runname.fct = factor(runname, labels = c("Without TDA", "With TDA"))) %>% 
# 	ggplot(aes(x = year, y = FR_MA/100, color = runname.fct)) + theme_bw()+
# 	geom_line() + 
# 	geom_point() + 
# 	coord_cartesian(ylim = c(0,1.5)) + 
# 	scale_y_continuous(breaks = seq(0, 2, 0.20), labels = percent) +
# 	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
# 	labs(title =    "Funded ratio with different treatment of TDA",
# 			 subtitle = "sim #424; 30-year geometric return = 7.0%",
# 			 color = NULL,
# 			 x = NULL, 
# 			 y = "Funded ratio") +
# 	RIG.theme()
# fig_TDA_compareFR 


# Figure 9 ####
fig9_TDA_compareERC <- 
df_singleRuns %>% 
	filter(year >= 2019) %>% 
	mutate(runname.fct = factor(runname, labels = c("Without TDA", "With TDA"))) %>% 
	ggplot(aes(x = year, y = ERC_PR/100, color = runname.fct, shape = runname.fct)) + theme_marron() + 
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0,0.50)) + 
	scale_y_continuous(breaks = seq(-1,2, 0.10), labels = function(x) percent(x, accuracy = 1)) + 
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title =    "Employer contribution rate with different treatment of TDA",
			 subtitle = "sim #424; 30-year geometric return = 7.0%",
			 color = NULL,shape = NULL,
			 x = NULL, y = "Employer contribution Rate") + 
	theme(legend.position = "bottom" )
fig9_TDA_compareERC 


save_figure(fig8_TDA_compareReturn, width = 2*3.7, height = 1.2*3.7, dpi = fig_dpi)
# save_figure(fig_TDA_compareFR,   width = 2*5, height = 1*5, dpi = fig_dpi)
save_figure(fig9_TDA_compareERC,    width = 2*3.7, height = 1.2*3.7, dpi = fig_dpi)


runs_RS1 <-   c("multiTier_noTDA_OYLM",
								"multiTier_TDAamortAS_OYLM",
								"multiTier_noTDA_OYLM_low15",
								"multiTier_TDAamortAS_OYLM_low15"
								#"multiTier_noTDA_OYLM_low30",
								#"multiTier_TDAamortAS_OYLM_low30"
								)


# Figure: Risk of high ERC (3 lines in a single graph)
# Figure 10 ####
fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time up to the given year\nunder alternative return scenarios"
fig.subtitle <- NULL #"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig10_TDA_ERChigh <- df_all.stch %>% 
	filter(runname %in% runs_RS1, year >= 2019) %>% 
	select(runname.fct, year, TDA_policy, return_scenario, ERC_high) %>% 
	mutate(
		return_scenario = factor(return_scenario, 
														 levels = c("RS1", "RS_15low"),
														 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
		TDA_policy = factor(TDA_policy, 
												levels = c("noTDA", "TDAamortAS with OYLM"),
												labels = c("Without TDA", "With TDA"))) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_high/100, color = TDA_policy, shape = TDA_policy )) + theme_marron() +
	facet_grid(.~return_scenario) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,1)) + 
	scale_y_continuous(breaks = seq(0,2, 0.10), labels = percent) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	# scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	# scale_shape_manual(values = c(17,16, 15),  name = "") +
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability",
			 color = NULL,
			 shape = NULL) + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) 
fig10_TDA_ERChigh



# Figure: Risk of ERC hike (3 lines in a single graph)
# Figure 11 ####
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time up to the given year\nunder alternative return scenarios"
fig.subtitle <- NULL# "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig11_TDA_ERChike <- df_all.stch %>% 
	filter(runname %in% runs_RS1, year >= 2019) %>% 
	select(runname.fct, TDA_policy, return_scenario, year, ERC_hike) %>% 
	mutate(
				 return_scenario = factor(return_scenario, 
				 												 levels = c("RS1", "RS_15low"),
				 												 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
				 TDA_policy = factor(TDA_policy, 
				 										levels = c("noTDA", "TDAamortAS with OYLM"),
				 										labels = c("Without TDA", "With TDA"))) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike / 100, color = TDA_policy, shape = TDA_policy)) + theme_marron() + 
	facet_grid(.~return_scenario) + 
	geom_point(size = 2) + geom_line() +  
	coord_cartesian(ylim = c(0,1)) + 
	scale_y_continuous(breaks = seq(0,2, 0.10), labels = percent) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	# scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") + 
	# scale_shape_manual(values = c(17,16, 15),  name = "") +
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability",
			 color = NULL,
			 shape = NULL) + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) 
fig11_TDA_ERChike


# Figure 12 ####
fig.title <- "Probability of funded ratio below 40% in any year up to the given year\nunder alternative return scenarios"
fig.subtitle <- NULL #"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig12_TDA_FR40less_2fig <- df_all.stch %>%
	filter(runname %in% runs_RS1, year >= 2019) %>%
	select(runname.fct, TDA_policy, return_scenario, year, FR40less, FR50less) %>%
	mutate(
				 return_scenario = factor(return_scenario,
				 												 levels = c("RS1", "RS_15low"),
				 												 labels = c("Baseline: Expected return = 7% ", "15 years of low returns")),
				 TDA_policy = factor(TDA_policy,
				 										levels = c("noTDA", "TDAamortAS with OYLM"),
				 										labels = c("Without TDA", "With TDA"))
	) %>%
	ggplot(aes(x = year, y = FR40less/100, color =TDA_policy, shape = TDA_policy)) + theme_marron() +
	facet_grid(.~ return_scenario) +
	geom_point(size = 2) + geom_line() +
	coord_cartesian(ylim = c(0,0.60)) +
	scale_y_continuous(breaks = seq(0,2, 0.1), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	# scale_color_manual(values = c("black",RIG.blue, RIG.red),  name = "") +
	# scale_shape_manual(values = c(17,16,15),  name = "") +
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability",
			 color = NULL,
			 shape = NULL) +
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) 
fig12_TDA_FR40less_2fig

save_figure(fig10_TDA_ERChigh,       w = 2.1*4, h = 1*4, dpi = fig_dpi)
save_figure(fig11_TDA_ERChike,       w = 2.1*4, h = 1*4, dpi = fig_dpi)
save_figure(fig12_TDA_FR40less_2fig, w = 2.1*4, h = 0.97*4, dpi = fig_dpi)



#*****************************************************
## Analysis 3 Asset schock scenarios              ####
#*****************************************************

# Figure 7 ####

fig.title <- "TRS funded ratio under hypothetical asset shock scenario"
fig.subtitle <- NULL
fig7a_shock1_FR <- 
results_all %>% 
	filter(runname %in% runs_DF[1:2], sim == 1, year >=2019) %>% 
	select(runname, year, FR_MA) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[1:2], labels = runs_DF_labels[1:2])) %>% 
	ggplot(aes(x = year,
						 y = FR_MA/100,
						 color = runname.fct,
				     shape = runname.fct)) + theme_marron() + 
	geom_line() +
	geom_point(size = 2) +
	geom_hline(yintercept = 1, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,1)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 5, 0.2), labels = percent) +
	# scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	# scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Funded ratio",
			 color = NULL,
			 shape = NULL) +
	theme(axis.text.x = element_text(size = 8),
				legend.position = "bottom",
				legend.key.width = unit(0.5, "inch"))
fig7a_shock1_FR



fig.title <- "TRS employer contribution rate under hypothetical asset shock scenario"
fig.subtitle <- NULL
fig7b_shock1_ERC <- 
	results_all %>% 
	filter(runname %in% runs_DF[1:2], sim == 1, year >= 2019) %>% 
	select(runname, year, ERC_PR) %>% 
	mutate(runname.fct = factor(runname, levels = runs_DF[1:2], labels = runs_DF_labels[1:2])) %>% 
	ggplot(aes(x = year,
						 y = ERC_PR/100,
						 color = runname.fct,
						 shape = runname.fct)) + theme_marron() +
	geom_line() +
	geom_point(size = 2) +
	coord_cartesian(ylim = c(0,0.6)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) +
	scale_y_continuous(breaks = seq(0, 5, 0.10), labels = function(x) percent(x, accuracy = 1)) +
	# scale_color_manual(values = c("black", RIG.blue, RIG.red, "red"),  name = NULL) +
	# scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Employer contribution as a percentage of payroll",
			 color = NULL,
			 shape = NULL) +
	theme(axis.text.x = element_text(size = 8),
				legend.position = "bottom",
				legend.key.width = unit(0.5, "inch" ))
fig7b_shock1_ERC


save_figure(fig7b_shock1_ERC, w = 1.6*4, h = 1*4, dpi = fig_dpi)
save_figure(fig7a_shock1_FR,  w = 1.6*4, h = 1*4, dpi = fig_dpi)



#***********************************************************************
## Analysis 4 all alternative TDA scenarios in one figure         ####
#***********************************************************************

# Examine the size of TDA fixed fund relative to QPP in various growth scenarios. 

df_TDA_alt <- 
results_all %>% 
	filter(sim == 0, runname %in% c("multiTier_TDAamortAS_OYLM",
																	"multiTier_TDAamortAS_OYLM_TDA_highG",
																	"multiTier_TDAamortAS_OYLM_TDA_lowG",
																	"multiTier_TDAamortAS_OYLM_TDA_LowRate1_base",
																	"multiTier_TDAamortAS_OYLM_TDA_LowRate1_lowG",
																	"multiTier_TDAamortAS_OYLM_TDA_LowRate2_base",
																	"multiTier_TDAamortAS_OYLM_TDA_LowRate2_lowG"
																	),
				 year %in% c(2016, 2030, 2048)
	)%>% 
	select(runname, year,  MA.TDA, MA, AL, PR) %>% 
	mutate(
		MA.TDA_AL = MA.TDA / AL,
		# MA.TDA_PR = MA.TDA / PR,
		MA.TDA_MA = MA.TDA / MA,
		) %>% 
	arrange(year) %>% 
	select(runname, year, MA.TDA_MA, MA.TDA_AL)

runs_TDA_alt <-  c(
	                 "multiTier_TDAamortAS_OYLM",
									 "multiTier_TDAamortAS_OYLM_TDA_highG",
									 "multiTier_TDAamortAS_OYLM_TDA_lowG",
									 "multiTier_TDAamortAS_OYLM_TDA_LowRate1_base",
									 "multiTier_TDAamortAS_OYLM_TDA_LowRate1_lowG",
									 "multiTier_TDAamortAS_OYLM_TDA_LowRate2_base",
									 "multiTier_TDAamortAS_OYLM_TDA_LowRate2_lowG",
									 "multiTier_noTDA_OYLM")

runs_TDA_alt_labels <- c(
	"7% TDA fixed rate;\nBaseline TDA net cash flow",
	"7% TDA fixed rate;\nhigh TDA net cash flow",
	"7% TDA fixed rate;\nlow  TDA net cash flow",
	"5% TDA fixed rate;\nBaseline TDA net cash flow",
	"5% TDA fixed rate;\nlow TDA net cash flow",
	"2.5% TDA fixed rate;\nBaseline TDA net cash flow",
	"2.5% TDA fixed rate;\nlow TDA net cash flow",
	"Without TDA")




# Figure 15 ####
# Figure: Risk of high ERC (3 lines in a single graph)
fig.title <- "Probability of employer contribution rising above 60% of payroll \nat any time up to the given year\nunder alternative TDA scenarios"
fig.subtitle <- NULL #"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig15_TDA_alt_ERChigh <- df_all.stch %>%
	filter(runname %in% runs_TDA_alt, year >= 2019) %>%
	select(runname, year, TDA_policy, return_scenario, ERC_high) %>%
	mutate(runname.fct = factor(runname, levels = runs_TDA_alt, labels = runs_TDA_alt_labels)) %>%
	ggplot(aes(x = year, y = ERC_high/100, color = runname.fct, shape = runname.fct )) + theme_marron() +
	# facet_grid(.~return_scenario) +
	geom_point(size = 2) + geom_line() +
	coord_cartesian(ylim = c(0,0.30)) +
	scale_y_continuous(breaks = seq(0,2, 0.05), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(seq(2020, 2045, 5), 2048)) +
	scale_color_manual(values = c(marron_colors, "black"),  name = "") +
	scale_shape_manual(values = c(16, 17,18, 0, 1,2,5, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability") +
	guides(color = guide_legend(keywidth = 1.5, keyheight = 2.5)) 
fig15_TDA_alt_ERChigh



# Figure 16 ####
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time up to the given year\nunder alternative TDA scenarios"
fig.subtitle <- NULL# "Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig16_TDA_alt_ERChike <- df_all.stch %>% 
	filter(runname %in% runs_TDA_alt, year >= 2019) %>% 
	select(runname, TDA_policy, return_scenario, year, ERC_hike) %>% 
	mutate(runname.fct = factor(runname, levels = runs_TDA_alt, labels = runs_TDA_alt_labels)) %>% 
	ggplot(aes(x = year, y = ERC_hike/100, color = runname.fct, shape = runname.fct)) + theme_marron()	+ 
	# facet_grid(.~return_scenario) + 
	geom_point(size = 2) + geom_line() +  
	coord_cartesian(ylim = c(0,1)) + 
	scale_y_continuous(breaks = seq(0,2, 0.1), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c(marron_colors,  "black"),  name = "") + 
	scale_shape_manual(values = c(16, 17,18, 0, 1,2,5, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 2.5))+
	RIG.theme()
fig16_TDA_alt_ERChike



# Figure 17 ####
fig.title <- "Probability of funded ratio below 40% in any year up to the given year\nunder alternative TDA scenarios"
fig.subtitle <- NULL #"Assumption achieved; expected compound return = 7% (w/o TDA transfer)"
fig17_TDA_alt_FR40less <- df_all.stch %>% 
	filter(runname %in% runs_TDA_alt, year >= 2019) %>% 
	select(runname, TDA_policy, return_scenario, year, FR40less) %>%
	mutate(runname.fct = factor(runname, levels = runs_TDA_alt, labels = runs_TDA_alt_labels)) %>% 
	ggplot(aes(x = year, y = FR40less/100, color =runname.fct, shape = runname.fct)) + theme_marron() + 
	# facet_grid(.~ return_scenario) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,0.40)) + 
	scale_y_continuous(breaks = seq(0,2, 0.05), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c(marron_colors,   "black"),  name = "") + 
	scale_shape_manual(values = c(16, 17,18, 0, 1, 2, 5, 15),  name = "") +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, y = "Probability") + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 2.5))
fig17_TDA_alt_FR40less



save_figure(fig15_TDA_alt_ERChigh,  w = 2*4, h = 1.3*4 ,dpi = fig_dpi)
save_figure(fig16_TDA_alt_ERChike,  w = 2*4, h = 1.3*4 ,dpi = fig_dpi)
save_figure(fig17_TDA_alt_FR40less, w = 2*4, h = 1.3*4 ,dpi = fig_dpi)




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

# Figure 14 ####

fig.title <- "Probability of funded ratio below 40% \nin any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
fig14_lowFR.fPolicy <- 
	df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 8, 11)], year >=2019) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(8, 11, 1)], 
															labels = c("No TDA,\n Open 30-year amort.",
																				 "No TDA,\n Closed 15-year amort.",
																				 "TDA, \nClosed 15-year amort. \n(TRS policy)"))) %>% 
	select(runname.fct, year, FR40less, FR50less) %>% 
	ggplot(aes(x = year, y = FR40less/100, color = runname.fct, shape = runname.fct)) + theme_marron() + 
	#facet_grid(.~ variable) + 
	geom_point(size = 1.9) + geom_line() +
	geom_segment(mapping=aes(x=2046.5, y=0.14, xend=2046.5, yend=0.02), arrow=arrow(angle = 25, type = "closed",length = unit(0.3, "cm")), size=0.8, color= marron_red) +
	geom_segment(mapping=aes(x=2048, y=0.02, xend=2048, yend=0.26), arrow=arrow(angle = 25, type = "closed",length = unit(0.3, "cm")), size=0.8, color= marron_blue2 ) + 
	geom_text(aes(x = c(2044), y = c(0.08), label = c("Effect of\nclosed\namort.")), color = marron_red, size = 4.5, family = "Gotham Book")+
	geom_text(aes(x = c(2045.5), y = c(0.20), label = c("Effect of\nTDA")), color =  marron_blue2, size = 4.5, family = "Gotham Book")+
	
	coord_cartesian(ylim = c(0,0.35),
									xlim = c(2019, 2048), 
									clip = "off") + 
	scale_y_continuous(breaks = seq(0,2, 0.05), label = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black", "grey40",  "black", "grey60",  RIG.blue, RIG.red, RIG.green)) + 
	# scale_color_manual(values = marron_colors) +
	scale_shape_manual(values = c(1,16, 15, 21, 18, 19, 20)) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, 
			 y = "Probability",
			 color = NULL,
			 shape = NULL) + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	theme(legend.position = c(0.01,0.99),
				legend.justification = c(0,1),
				legend.background = element_rect(fill="white", 
																				 size=0.5, linetype="solid", color = "grey70"),
				plot.margin = margin(t = 2, r =95, b = 2, l = 2, unit = "pt")
				) + 
	geom_text(aes(x = c(2050), y = c(0.025), label = c("Conservative\nfunding policy,")), color =  marron_red, size = 3, hjust = 0, family = "Gotham Book") + 
  geom_text(aes(x = c(2050), y = c(0.003),  label = c("No TDA")), color =  "grey40", size = 3, hjust = 0, family = "Gotham Book") + 
  
	geom_text(aes(x = c(2050), y = c(0.16), label = c("Risky funding policy,\nNo TDA")), color = "grey40", size = 3, hjust = 0, family = "Gotham Book")+
	
  geom_text(aes(x = c(2050), y = c(0.30), label = c("Current TRS policy:")), color = "black", size = 3, hjust = 0, family = "Gotham Book") + 
	geom_text(aes(x = c(2050), y = c(0.30 - 0.022),  label = c("Conservative\nfunding policy,")), color =  marron_red, size = 3, hjust = 0, family = "Gotham Book") +  
  geom_text(aes(x = c(2050), y = c(0.30 - 0.045),  label = c("Plus TDA")), color =  marron_blue2, size = 3, hjust = 0, family = "Gotham Book")  
 fig14_lowFR.fPolicy



# Figure 13 ####
# Figure: Risk of ERC hike (3 lines in a single graph)

fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7% (before TDA transfer)"
fig13_ERChike.fPolicy <- df_all.stch %>% 
	filter(runname %in% runs_fPolicy1[c(1, 8, 11)], year >= 2019) %>% 
	mutate(runname.fct = factor(runname, levels = runs_fPolicy1[c(8, 11, 1)], 
															labels = c("No TDA,\n Open 30-year amort.",
																				 "No TDA,\n Closed 15-year amort.",
																				 "TDA, \nClosed 15-year amort. \n(TRS policy)"))) %>%	select(runname.fct, year, ERC_hike) %>% 
	#mutate(ERChike.det = 0) %>% 
	# gather(type, value, -year, -runname) %>% 
	ggplot(aes(x = year, y = ERC_hike/100, color = runname.fct, shape = runname.fct)) + theme_marron() + 
	geom_point(size = 2) + geom_line() + 
	geom_segment(mapping=aes(x=2047, y=0.11, xend=2047, yend=0.52), arrow=arrow(angle = 25, type = "closed",length = unit(0.3, "cm")), size=0.8, color=marron_red) +
	geom_segment(mapping=aes(x=2047, y=0.57, xend=2047, yend=0.72), arrow=arrow(angle = 25, type = "closed",length = unit(0.3, "cm")), size=0.8, color=marron_blue2) + 
	geom_text(aes(x = c(2044), y = c(0.25), label = c("Effect of\nclosed\namort.")), color = marron_red, size = 4.5, family = "Gotham Book")+
	geom_text(aes(x = c(2044), y = c(0.58), label = c("Effect of\nTDA")), color = marron_blue2, size = 4.5, family = "Gotham Book")+
	coord_cartesian(ylim = c(0,1),
									xlim = c(2019, 2048), 
									clip = "off") + 
	scale_y_continuous(breaks = seq(0,2, 0.10), labels = percent) +
	scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5), 2048)) + 
	scale_color_manual(values = c("black", "grey40",  "black", "grey60",  RIG.blue, RIG.red, RIG.green)) + 
	scale_shape_manual(values = c(1,16, 15, 21, 18, 19, 20)) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = NULL, 
			 y = "Probability",
			 color = NULL,
			 shape = NULL) + 
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
	theme(legend.position = c(0.01,0.99),
				legend.justification = c(0,1),
				legend.background = element_rect(fill="white", 
																				 size=0.5, linetype="solid", color = "grey70"),
				plot.margin = margin(t = 2, r =95, b = 2, l = 2, unit = "pt")) + 
	
	geom_text(aes(x = c(2050), y = c(0.1), label = c("Risky funding policy,\nNo TDA")), color = "grey40", size = 3, hjust = 0, family = "Gotham Book")+
	
	geom_text(aes(x = c(2050), y = c(0.6), label = c("Conservative\nfunding policy,")), color =  marron_red, size = 3, hjust = 0, family = "Gotham Book") + 
	geom_text(aes(x = c(2050), y = c(0.6 - 0.022/0.36),  label = c("No TDA")), color =  "grey40", size = 3, hjust = 0, family = "Gotham Book") + 
	
	geom_text(aes(x = c(2050), y = c(0.88), label = c("Current TRS policy:")), color = "black", size = 3, hjust = 0, family = "Gotham Book") + 
	geom_text(aes(x = c(2050), y = c(0.88 - 0.022/0.36),  label = c("Conservative\nfunding policy,")), color =  marron_red, size = 3, hjust = 0, family = "Gotham Book") +  
	geom_text(aes(x = c(2050), y = c(0.88 - 0.045/0.36),  label = c("Plus TDA")), color =  marron_blue2, size = 3, hjust = 0, family = "Gotham Book")  

fig13_ERChike.fPolicy


save_figure(fig13_ERChike.fPolicy, w = 1.75*4, h = 1.3*4, dpi = fig_dpi)
save_figure(fig14_lowFR.fPolicy,   w = 1.75*4, h = 1.3*4, dpi = fig_dpi)




#***********************************************************************
## Historical data   ####
#***********************************************************************


# Figure 1 ####
df_fig12 <- read_excel("Inputs_data/DataExcelFig/NYCTRS_ExcelFiguresToBeConvertedToRForReport_2020-03-13(1).xlsx", 
											 sheet = "F1.F2_FundedRatio_data", skip = 1) %>% 
	          select(year = 1, FR_GASB = 8, MVA_QPP = 6, AL_QPP = 7)



fig1_labelYear <- c(2000, 2003, 2007, 2009, 2011, 2013, 2015, 2018)
fig1_annotation <- 
	data.frame(
		x = fig1_labelYear,
		y = filter(df_fig12, year %in% fig1_labelYear)$FR_GASB
	)


fig.title <- "Funded ratio (GASB)"
fig1 <- ggplot(df_fig12, aes(x = year, y = FR_GASB)) + theme_marron() + 
					geom_line(color = marron_orange) + 
					geom_point(color = marron_orange, size = 2) +
					coord_cartesian(ylim = c(0,1.2),
													clip = "off") + 
					scale_y_continuous(breaks = seq(0,1.2, 0.20), labels = function(x) percent(x, accuracy = 1)) +
					scale_x_continuous(breaks = c(2000:2018)) +
					labs(title = fig.title,
							 #subtitle = fig.subtitle,
							 x = NULL, 
							 y = NULL) +
	        geom_text(data = fig1_annotation, aes(x = x, y = y + 0.05, label = paste0(round(y*100), "%")),
						        family = "Gotham Book", size = 3)
fig1
save_figure(fig1,   w = 1.9*4, h = 1.3*4, dpi = fig_dpi)


# Figure 2 ####

fig2_annotation <- 
data.frame(
	x    = c(2000, 2002, 2006, 2009, 2012, 2014),
	xend = c(2000, 2002, 2006, 2009, 2012, 2014),
	y    = rep(0, 6),
	yend = filter(df_fig12, year %in% c(2000, 2002, 2006, 2009, 2012, 2014))$AL_QPP/1e6,
	y_text = filter(df_fig12, year %in% c(2000, 2002, 2006, 2009, 2012, 2014))$AL_QPP/1e6 +6,
	label = c("Benefit\nincreases\n2000",
						"Early\n2000s\nrecession",
						"Actuarial\nChange\n2006",
						"Great\nRecession\n2008-2010",
						"Actuarial\nChanges\nTier VI\n2012",
						"GASB 67/68")
)
fig2_annotation$y_text <- fig2_annotation$y_text + c(0,0, 0, 2.5, 0.5, -0.5)

fig2_bars <- filter(df_fig12, year > 2000 ) %>% 
	mutate(y = 0,
				 yend = y + 20,
				 x = year,
				 xend = year)

df_fig2 <- 
  df_fig12 %>% 
	select(year, MVA_QPP, AL_QPP) %>% 
	gather(Var, value, -year) %>% 
	mutate(Var = factor(Var, levels = c("MVA_QPP", "AL_QPP"),
											labels = c("QPP Market Value of Assets", "QPP Actuarial Liabilities")))


fig.title <- "TRS defined benefit plan market-value assets and actuarial liabilities \n($billion)"
fig2 <- 
	ggplot(df_fig2, aes(x = year, y = value/1e6, color = Var)) + theme_marron() +
	geom_line() +
	geom_point(size = 2) +
	
	# ggplot() +  theme_marron() + 
	# geom_line(data = df_fig2,  aes(x = year, y = value/1e6, color = Var)) + 
	# geom_point(data = df_fig2, aes(x = year, y = value/1e6, color = Var), size = 2) +
	
	coord_cartesian(ylim = c(0,80)) +
	scale_y_continuous(breaks = seq(0,80, 10), labels = function(x) paste0("$",x)) +
	scale_x_continuous(breaks = c(2000:2018)) +
	scale_color_manual(values = c(marron_brown2, marron_blue1)) +
	# scale_shape_manual(values = c(1,16, 15, 21, 18, 19, 20)) +
	labs(title = fig.title,
			 #subtitle = fig.subtitle,
			 x = NULL,
			 y = NULL,
			 colour = NULL,
			 shape = NULL) +
	theme(legend.position = "bottom") +
	annotate(geom = "text", family = "Gotham Book", size = 2.5,
		       x = fig2_annotation$x, y = fig2_annotation$y_text,
					 label = fig2_annotation$label) +
  geom_segment(data = fig2_annotation,
						 mapping=aes(x=x, y=y, xend=xend, yend=yend), size=0.6, color="grey20", linetype = 2, alpha = 0.6) +
	annotate("segment",
					 x = fig2_bars$year,
					 y = fig2_bars$MVA_QPP/1e6*1.015,
					 xend = fig2_bars$year,
					 yend = fig2_bars$AL_QPP/1e6 *0.985,
					 size = 4,
					 color= marron_orange,
					 alpha = 0.4)
fig2

save_figure(fig2,   w = 1.9*4, h = 1.3*4, dpi = fig_dpi)

# https://stackoverflow.com/questions/31568453/using-different-font-styles-in-annotate-ggplot2


# Figure 6 ####
df_fig6 <- read_excel("Inputs_data/DataExcelFig/NYCTRS_ExcelFiguresToBeConvertedToRForReport_2020-03-13(1).xlsx", 
											 sheet = "F6_TDA_graph_data", skip = 2) %>% 
	select(year = 1, r_portfolio = 2, r_effective = 3) %>% 
	filter(year < 2018)
df_fig6


fig.title <- "Impact of TDA on QPP effective returns, 2008 through 2017"
fig6 <- 
	df_fig6 %>% 
	select(year, r_portfolio, r_effective) %>% 
	gather(Var, value, -year) %>% 
	mutate(Var = factor(Var, levels = c("r_portfolio", "r_effective"),
											labels = c("Return on investment portfolio", "QPP effective rate of return"))) %>% 
	ggplot(aes(x = year, y = value, color = Var, shape  = Var)) + theme_marron() + 
	geom_line() + 
	geom_point(size = 2) +
	geom_hline(yintercept = 0.072, linetype = 2, size = 0.4) + 
	coord_cartesian(ylim = c(-0.3,0.35),
									clip = "off") + 
	scale_y_continuous(breaks = seq(-1,1, 0.05), labels = function(x) percent(x, accuracy = 1)) +
	scale_x_continuous(breaks = c(2008:2017)) +
	# scale_color_manual(values = c(marron_blue1, marron_orange)) + 
	scale_color_manual(values = c(marron_blue2, marron_blue1)) + 
	scale_shape_manual(values = c(1, 16)) + 
	labs(title = fig.title,
			 #subtitle = fig.subtitle,
			 x = NULL, 
			 y = NULL,
			 colour = NULL,
			 shape = NULL) +
	theme(legend.position = "bottom") + 
	annotate(geom = "text", family = "Gotham Book", size = 3, 
					 x = 2008.2, y = 0.087,
					 label = "Guarantee") 
fig6

save_figure(fig6,   w = 1.9*4, h = 1.3*4, dpi = fig_dpi)




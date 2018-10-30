# Load demographic data of large teachers' plans for imputation of TRS actives 

# Notes:
		# Large plans data are collected for the Pension Simulation Project - modeling 170 plans

# 1. Load data
# 2. Compare age and yos distributions. (age distributions also compared against NYCTRS)
# 3. Construct an average age-yos distribution





#*********************************************************************************************************
#                      ## Global settings  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/DataLargePlans/"



#*********************************************************************************************************
#                      ## 1. Loading data  ####
#*********************************************************************************************************


# NYSTRS 

df_NYSTRS <- read_xlsx(paste0(dir_data, "78_NY_NY-NYSTRS.xlsx"), sheet = "ActivesSched", range = "B9:O33")

df_NYSTRS %<>% 
	filter(!is.na(type)) %>% 
  #mutate(keyVar = paste0(type, age.cell)) %>% 
	gather(yos.cell, value, -type, -age.cell, -agegrp)

df_yosgrp <- df_NYSTRS %>% filter(type == "yosgrp") %>% select(yos.cell, yosgrp = value)

df_NYSTRS %<>% 
	filter(type != "yosgrp") %>% 
	left_join(df_yosgrp) %>% 
	mutate_at(vars(age.cell, yos.cell, value), funs(as.numeric)) %>% 
	mutate(plan = "NYSTRS") %>% 
	select(plan, type, age.cell, agegrp, yos.cell, yosgrp, value) %>% 
	arrange(type, age.cell, yos.cell)

df_NYSTRS


# TX-TRS 

df_TXTRS <- read_xlsx(paste0(dir_data, "108_TX_TX-TRS.xlsx"), sheet = "ActivesSched", range = "B7:L28")

df_TXTRS %<>% 
	filter(!is.na(type)) %>% 
	gather(yos.cell, value, -type, -age.cell, -agegrp)

df_yosgrp <- df_TXTRS %>% filter(type == "yosgrp") %>% select(yos.cell, yosgrp = value)

df_TXTRS %<>% 
	filter(type != "yosgrp") %>% 
	left_join(df_yosgrp) %>% 
	mutate_at(vars(age.cell, yos.cell, value), funs(na2zero(as.numeric(.)))) %>% 
	mutate(plan = "TXTRS") %>%
	select(plan, type, age.cell, agegrp, yos.cell, yosgrp, value) %>% 
	arrange(type, age.cell, yos.cell)

df_TXTRS



# OH-STRS

df_OHSTRS <- read_xlsx(paste0(dir_data, "88_OH_OH-STRS.xlsx"), sheet = "ActivesSched", range = "B9:M32")

df_OHSTRS %<>% 
	filter(!is.na(type)) %>% 
	gather(yos.cell, value, -type, -age.cell, -agegrp)

df_yosgrp <- df_OHSTRS %>% filter(type == "yosgrp") %>% select(yos.cell, yosgrp = value)

df_OHSTRS %<>% 
	filter(type != "yosgrp") %>% 
	left_join(df_yosgrp) %>% 
	mutate_at(vars(age.cell, yos.cell, value), funs(na2zero(as.numeric(.)))) %>% 
	mutate(plan = "OHSTRS") %>%
	select(plan, type, age.cell, agegrp, yos.cell, yosgrp, value) %>% 
	arrange(type, age.cell, yos.cell)

df_OHSTRS


# GATRS

df_GATRS <- read_xlsx(paste0(dir_data, "28_GA_GA-TRS.xlsx"), sheet = "ActivesSched", range = "B8:M33")

df_GATRS %<>% 
	filter(!is.na(type)) %>% 
	gather(yos.cell, value, -type, -age.cell, -agegrp)

df_yosgrp <- df_GATRS %>% filter(type == "yosgrp") %>% select(yos.cell, yosgrp = value)

df_GATRS %<>% 
	filter(type != "yosgrp") %>% 
	left_join(df_yosgrp) %>% 
	mutate_at(vars(age.cell, yos.cell, value), funs(na2zero(as.numeric(.)))) %>% 
	mutate(plan = "GATRS") %>%
	select(plan, type, age.cell, agegrp, yos.cell, yosgrp, value) %>% 
	arrange(type, age.cell, yos.cell)

df_GATRS


# CALSTRS

df_CALSTRS <- read_xlsx(paste0(dir_data, "10_CA_CA-CALSTRS.xlsx"), sheet = "ActivesSched", range = "B7:N30")

df_CALSTRS %<>% 
	filter(!is.na(type)) %>% 
	gather(yos.cell, value, -type, -age.cell, -agegrp)

df_yosgrp <- df_CALSTRS %>% filter(type == "yosgrp") %>% select(yos.cell, yosgrp = value)

df_CALSTRS %<>% 
	filter(type != "yosgrp") %>% 
	left_join(df_yosgrp) %>% 
	mutate_at(vars(age.cell, yos.cell, value), funs(na2zero(as.numeric(.)))) %>% 
	mutate(plan = "CALSTRS") %>%
	select(plan, type, age.cell, agegrp, yos.cell, yosgrp, value) %>% 
	arrange(type, age.cell, yos.cell)

df_CALSTRS



df_largePlans <- 
	bind_rows(df_NYSTRS,
						df_CALSTRS,
						df_TXTRS,
						df_OHSTRS,
						df_GATRS)

df_largePlans


#*********************************************************************************************************
#                      ## 2.1 Comparing distributions of actives  ####
#*********************************************************************************************************

df_nactives_large <- 
	df_largePlans %>% 
	filter(type == "nactives") %>% 
	group_by(plan) %>% 
	mutate(nactives_share = 100 * value / sum(value))
df_nactives_large



# age distribution of TRS
load(paste0("Inputs_data/", 'Data_DemoCAFR17.RData'))
df_age_NYCTRS <- 
df_nactives %>% 
	mutate(plan = "NYCTRS",
				 age.cell = age_lb + 2,
				 nactives = nactives_male + nactives_female,
				 nactives_share = 100 * nactives/sum(nactives)) %>% 
	select(plan, age.cell, nactives_share)
df_age_NYCTRS


# Age distribution
df_nactives_large %>%
	group_by(plan, age.cell) %>% 
	summarise(nactives_share = sum(nactives_share)) %>% 
	bind_rows(df_age_NYCTRS) %>% 
	ggplot(aes(x = age.cell, y = nactives_share, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() + 
	scale_y_continuous(breaks = seq(0, 100, 2))
	
  # Observations
    # NYCTRS has lower shares for emplpoyees in age group 40-55 than large plans.  
    # GATRS has very high share for age group 45-50, and lower shares for younger age groups. 


# yos distribution
df_nactives_large %>%
	group_by(plan, yos.cell) %>% 
	summarise(nactives_share = sum(nactives_share)) %>% 
	ggplot(aes(x = yos.cell, y = nactives_share, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() + 
	scale_y_continuous(breaks = seq(0, 100, 2))

	# Observations
		# All plans have "bumps" at the yos group 10-15, possible reasons include: c
      # 10-year vesting requirement. But need to confirm
      # Natural decline of separation rates after 10 years. 



# yos distributions by age
df_nactives_large %>%
	group_by(plan, age.cell) %>% 
	ggplot(aes(x = yos.cell, y = nactives_share, color = plan)) + theme_bw() + 
	facet_wrap( ~age.cell) + 
	geom_point() + 
	geom_line() + 
	scale_y_continuous(breaks = seq(0, 100, 2))



#*********************************************************************************************************
#                      ## 2.2 Comparing distributions of salary  ####
#*********************************************************************************************************

df_salary_large   <- df_largePlans %>% filter(type == "salary") %>% rename(salary = value) %>% 
	left_join(df_nactives_large %>% select(age.cell, yos.cell, nactives = value))
df_salary_large

# age distribution of TRS
load(paste0("Inputs_data/", 'Data_DemoCAFR17.RData'))
df_sal_NYCTRS <- 
	df_nactives %>% 
	mutate(plan = "NYCTRS",
				 age.cell = age_lb + 2,
				 salary  = (nactives_male * salary_male + nactives_female * salary_female) / (nactives_male + nactives_female),
				 sal.avg = sum((nactives_male + nactives_female) * salary) / sum(nactives_male + nactives_female),
				 sal.scale = salary/sal.avg
				 ) %>% 
	select(plan, age.cell, salary, sal.avg, sal.scale)
df_sal_NYCTRS



# Age distribution, including NYCTRS
df_salary_large %>%
	group_by(plan, age.cell) %>% 
	summarise(salary = sum(salary * nactives) / sum(nactives),
						nactives = sum(nactives)) %>% 
	mutate( sal.avg = sum(nactives * salary) / sum(nactives),
					sal.scale = salary/sal.avg) %>% 
	bind_rows(df_sal_NYCTRS) %>% 
	ggplot(aes(x = age.cell, y = sal.scale, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() 
	#scale_y_continuous(breaks = seq(0, 100, 2))

	# Notes: GATRS has higher salary at low ages and lower salary at high ages.
         # Other Plans are quite comparable to NYCTRS


# yos distribution
df_salary_large %>%
	group_by(plan, yos.cell) %>% 
	summarise(salary = sum(salary * nactives) / sum(nactives),
						nactives = sum(nactives)) %>% 
	mutate( sal.avg = sum(nactives * salary) / sum(nactives),
					sal.scale = salary/sal.avg) %>% 
	ggplot(aes(x = yos.cell, y = sal.scale, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() 
#scale_y_continuous(breaks = seq(0, 100, 2))
  
  # Notes: GATRS has high salary for low yos


# yos distributions by age
df_salary_large %>% 
	group_by(plan, age.cell) %>% 
	mutate( sal.avg = sum(nactives * salary) / sum(nactives),
					sal.scale = salary/sal.avg) %>% 
	ggplot(aes(x = yos.cell, y = sal.scale, color = plan)) + theme_bw() + 
	facet_wrap( ~age.cell) + 
	geom_point() + 
	geom_line() 

#  Notes
	# Possible outliers:
  #   (most at the end of the distributions, maybe caused by small sample size in those cells)
	#  1. agegrp 27: OHSTRS yosgrp 12 
  #  2. agegrp 37: NYSTRS yosgrp 22
  #  3. agegrp 42: NYSTRS yosgrp 27
  #  4. agegrp 47: OHSTRS yosgrp 32
  #  5. agegrp 57: NYSTRS yosgrp 44
  #  5. agegrp 62: NYSTRS yosgrp 47




# Todo: 
  #  check salary scale of NYSTRS and NYCTRS



#*********************************************************************************************************
#           # 3. Construct average age-yos distributions for actives and salary    ####
#*********************************************************************************************************


# Distributions of actives
 # 1. Weighted: sum up the number of actives of all plans in each age-yos cell, 
 #    then calculate the share of each cell.
 # 2. Simple average: Calcuate the share of each age-yos cell for every plan, 
 #    then for each cell calcuate the simple average across all plans. 

# Salary distribution
 # 1. Weighted: For each age-yos, pool the members of all plans and calculate the average salaries, 
 #    then calculate the ratio of the salary in each cell to overall average salary 
 # 2. Simple average 1: For each plan, calculate the ratio of the salary in each cell to the overall salary,
 #    then for each cell calculate the simple average across all plans. 
#  2' Simple average 2: For each plan, calculate the ratio of the salary in each cell to the average salary of the age group,
#     then for each cell calculate the simple average across all plans. 

# Sensitivity tests:
# 1. Excluding plans that may be significantly different from others: GATRS, CALSTRS
# 2. Excluding possible outliers

# To do:
   # 1. calculate all average distributions
   # 2. make small adjsutments
   # 3. do sensitivity check
   # 4. summarize and better document

df_large <- df_salary_large

# index tables for age cells and yos cells
agecuts <- df_large %>% select(age.cell, agegrp) 
agecuts <- agecuts[!duplicated(agecuts), ]
agecuts %<>% 
	separate(agegrp, into = c("agelb", "ageub")) %>% 
  mutate_all(funs(as.numeric))

yoscuts <- df_large %>% select(yos.cell, yosgrp)
yoscuts <- yoscuts[!duplicated(yoscuts), ]
yoscuts %<>% 
	separate(yosgrp, into = c("yoslb", "yosub")) %>% 
  mutate_all(funs(as.numeric))

	

# 3.1 Distribution of actives ####
  
  # Distributions of actives
  	# 1. Weighted: sum up the number of actives of all plans in each age-yos cell, 
  	#    then calculate the share of each cell.
  	# 2. Simple average: Calcuate the share of each age-yos cell for every plan, 
  	#    then for each cell calcuate the simple average across all plans. 

scale_nactives <- 
left_join(
    df_large %>% # weighted
    	group_by(age.cell, yos.cell) %>% 
    	summarise(nactives = sum(nactives)) %>% 
    	ungroup %>% 
    	mutate(nactives_share_w = nactives / sum(nactives)) %>% 
    	select(-nactives),
    
    df_large %>% # simple 
    	group_by(plan) %>% 
    	mutate(nactives_share = nactives / sum(nactives)) %>% 
    	group_by(age.cell, yos.cell) %>% 
    	summarise(nactives_share_s = mean(nactives_share, na.rm = TRUE))
)
scale_nactives


# Plot and compare the scales by age group: very similar 
scale_nactives %>% 
	gather(scaleType, value, -age.cell, -yos.cell) %>% 
	ggplot(aes(x = yos.cell, y = value, color = scaleType)) + theme_bw() +
	facet_wrap(~age.cell) + 
	geom_line()+
	geom_point()


# Compare the overal age distributions against TRS
  # TRS has a younger age distribution than the estimated distributions
  # Possible implications on the distributions of YOS by age?

scale_nactives %>%
	group_by(age.cell) %>%
	summarise(
		nactives_share_w = 100 * sum(nactives_share_w),
		nactives_share_s = 100 * sum(nactives_share_s)
	) %>%
	left_join(df_age_NYCTRS %>% rename(nactives_share_TRS = nactives_share)) %>%
	gather(type, value,-age.cell,-plan) %>%
	ggplot(aes(x = age.cell, y = value, color = type)) + theme_bw() +
	geom_line() +
	geom_point()

	
# 3.2 Distribution of salary ####

# Salary distribution
  # 1. Weighted: For each age-yos, pool the members of all plans and calculate the average salaries, 
  #    then calculate the ratio of the salary in each cell to overall average salary 
  # 2. Simple average 1: For each plan, calculate the ratio of the salary in each cell to the overall salary,
  #    then for each cell calculate the simple average across all plans. 
  # 2' Simple average 2: For each plan, calculate the ratio of the salary in each cell to the average salary of the age group,
  #    then for each cell calculate the simple average across all plans. 

df_large

scale_salary <-
	left_join(
		df_large %>% # weighted
			group_by(age.cell, yos.cell) %>%
			summarise(
				nactives_cell = sum(nactives),
				salary_cell   = sum(nactives * salary) / sum(nactives),
			) %>%
			ungroup %>%
			mutate(
				sal.avg = sum(nactives_cell * salary_cell, na.rm = TRUE) / sum(nactives_cell),
				sal.scale_w = na2zero(salary_cell / sal.avg)
			) %>%
			select(age.cell, yos.cell, sal.scale_w) %>%
			ungroup,
		
		
		df_large %>% # simple
			group_by(plan) %>%
			mutate(
				sal.avgPlan    = sum(nactives * salary) / sum(nactives),
				sal.scale.plan = salary / sal.avgPlan
			) %>%
			group_by(age.cell, yos.cell) %>%
			summarize(sal.scale_s1 = mean(sal.scale.plan, na.rm = TRUE)) %>%
			ungroup) %>% 

	left_join(
		df_large %>% # simple
			group_by(plan, age.cell) %>%
			mutate(
				sal.avgPlanAge    = sum(nactives * salary) / sum(nactives),
				sal.scale.planAge = salary / sal.avgPlanAge
			) %>%
			group_by(age.cell, yos.cell) %>%
			summarize(sal.scale_s2 = mean(sal.scale.planAge, na.rm = TRUE)) %>%
			ungroup
	)


# Plot and compare the scales by age group: very similar 
scale_salary %>% 
    gather(scaleType, value, -age.cell, -yos.cell) %>% 
	    group_by(scaleType, age.cell) %>% 
	    mutate(value = value / mean(value)) %>% # all values standardized by age group mean 
    	ggplot(aes(x = yos.cell, y = value, color = scaleType)) + theme_bw() +
    	facet_wrap(~age.cell) + 
    	geom_line()+
    	geom_point()


# Compare the overall age distributions against TRS
  # Average across yos for each age group calculated in the spirit of simple average 2 
  # The average distribution is very similar to the TRS distribution. 
  # Better to include GATRS

df_salary_large %>%
	# filter(!plan %in% c("CALSTRS", "GATRS")) %>% 
	# filter(!plan %in% c("GATRS")) %>% # looks better to include GATRS
	group_by(plan, age.cell) %>% 
	summarise(salary = sum(salary * nactives) / sum(nactives),
						nactives = sum(nactives)) %>% 
	mutate( sal.avg = sum(nactives * salary) / sum(nactives),
					sal.scale = salary/sal.avg) %>% 
	group_by(age.cell) %>% 
	summarize(sal.scale = mean(sal.scale, na.rm = TRUE)) %>% 
	mutate(plan = "Average") %>% 
	bind_rows(df_sal_NYCTRS %>% select(plan, age.cell, sal.scale)) %>% 
	ggplot(aes(x = age.cell, y = sal.scale, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() 



#*********************************************************************************************************
#           # 4. Summary and next steps   ####
#*********************************************************************************************************

# Outputs
(scale_nactives_largePlans <- scale_nactives)
(scale_salary_largePlans   <- scale_salary)

save(scale_nactives_largePlans, scale_salary_largePlans, agecuts, yoscuts, 
		 file = paste0(dir_data, "../Scales_largePlans.RData"))



#1.Distributions of actives
  # Two averaging approaches:
    # 1. Weighted: sum up the number of actives of all plans in each age-yos cell, 
    #    then calculate the share of each cell.
    # 2. Simple average: Calcuate the share of each age-yos cell for every plan, 
    #    then for each cell calcuate the simple average across all plans. 
  
  # Examining the results
    # Plot and compare the scales by age group: very similar
    # Compare the overal age distributions against TRS
      # TRS has a younger age distribution than the estimated distributions
      # Possible implications on the distributions of YOS by age?               **** 


#2.Salary distribution
  # Three averaing approaches
    # 1. Weighted: For each age-yos, pool the members of all plans and calculate the average salaries, 
    #    then calculate the ratio of the salary in each cell to overall average salary 
    # 2. Simple average 1: For each plan, calculate the ratio of the salary in each cell to the overall salary,
    #    then for each cell calculate the simple average across all plans. 
    # 2' Simple average 2: For each plan, calculate the ratio of the salary in each cell to the average salary of the age group,
    #     then for each cell calculate the simple average across all plans. 


  # Examining the results 
  	# Plot and compare the scales by age group: very similar 
    # Compare the overall age distributions against TRS
      # Average across yos for each age group calculated in the spirit of simple average 2 
      # The average distribution is very similar to the TRS distribution. 
      # Better to include GATRS


# Todo :
  # remove implausible age-yos combos
  # standardize age-yos ranges across large plans




#*********************************************************************************************************
#  5. Preliminary Imputation    ####
#*********************************************************************************************************
# 
# df_NYCTRS <- 
# 	df_nactives %>% 
# 	mutate(plan = "NYCTRS",
# 				 age.cell = age_lb + 2,
# 				 nactives = nactives_male + nactives_female,
# 				 salary  = (nactives_male * salary_male + nactives_female * salary_female) / (nactives_male + nactives_female),
# 				 sal.avg = sum((nactives_male + nactives_female) * salary) / sum(nactives_male + nactives_female),
# 				 sal.scale = salary/sal.avg
# 	) %>% 
# 	select(plan, age.cell, nactives, salary, sal.avg)
# 
# df_NYCTRS
# 
# 
# scale_largePlans <- left_join(scale_nactives, scale_salary) %>% 
# 	select(age.cell, yos.cell, scale_nact = nactives_share_s, scale_sal = sal.scale_s2)
# 
# 
# df_NYCTRS_impt <- 
# scale_largePlans %>% 
# 	left_join(df_NYCTRS) %>% 
# 	group_by(age.cell) %>% 
# 	mutate(nactives_impt = nactives * scale_nact / sum(scale_nact, na.rm = TRUE),
# 				 salary_impt   = scale_sal * salary * sum(nactives_impt) / sum(scale_sal * nactives_impt)
# 				 )
# 
# df_NYCTRS_impt
# 	
# 
# # Double check
# df_NYCTRS_impt %>% # average salary by group
# 	group_by(age.cell) %>% 
# 	summarize(sal.avg = sum(nactives_impt * salary_impt)/sum(nactives_impt),
# 						sal.TRS = unique(salary))
# 
# df_NYCTRS_impt %>% ungroup %>% # Overall average Target: 78038.98
# 	summarize(sal.avg = sum(nactives_impt * salary_impt)/sum(nactives_impt))
# 








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
#                      ## Comparing distributions of actives  ####
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
#                      ## Comparing distributions of salary  ####
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
				 salary = (nactives_male * salary_male + nactives_female * salary_female) / (nactives_male + nactives_female)) %>% 
	select(plan, age.cell, salary)
df_sal_NYCTRS



# Age distribution
df_salary_large %>%
	group_by(plan, age.cell) %>% 
	summarise(salary = sum(salary * nactives) / sum(nactives) ) %>% 
	bind_rows(df_sal_NYCTRS) %>% 
	ggplot(aes(x = age.cell, y = salary, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() 
	#scale_y_continuous(breaks = seq(0, 100, 2))


# yos distribution
df_salary_large %>%
	group_by(plan, yos.cell) %>% 
	summarise(salary = sum(salary * nactives) / sum(nactives) ) %>% 
	ggplot(aes(x = yos.cell, y = salary, color = plan)) + theme_bw() + 
	geom_point() + 
	geom_line() 
#scale_y_continuous(breaks = seq(0, 100, 2))



# yos distributions by age
df_salary_large %>%
	group_by(plan, age.cell) %>% 
	ggplot(aes(x = yos.cell, y = salary, color = plan)) + theme_bw() + 
	facet_wrap( ~age.cell) + 
	geom_point() + 
	geom_line() 



# Todo: 
#  plot salary relative to average salary
#  check salary scale of NYSTRS and NYCTRS






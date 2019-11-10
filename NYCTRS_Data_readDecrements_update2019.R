# This script loads decrement tables from the 2015 experience study of NYCTRS


# General Comments



# Comments on data issues in experience study 2015 ####
 # consistency issue:
 # why assumed probability != expected # / Total Exposed

# TODO: 
  # need to check the difference between elected and mandated 
  # further check if the disability rates are accurate


# Notes on retirement rates of elected and mandated members
#   -  Upto age X, the combined rates are heavily weighted toward elected members
#   -  At ate X + 1, the combined rates are generally less weighted toward elected members (or even toward mandated members)
#   -  After age X + 1, the combined rates are the same as the mandated rates.
#   -  X = 
#          60 for 1st year eligibility
#          61 for 2nd year eligibility
#          62 for after 2nd year eligibility


#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/"
file_name <- "NYCTRS_Decrements&SalaryScale_ES2015.xlsx" 
file_path <- paste0(dir_data, file_name)
file_path



## 1. Mortality for Service Retirees ####

# - Assumed mortality rates are not provided.
# - Rates are calculated as "expected death under current assumption" (5) / "Total Exposure" (3)
# - Use 4-year period experience
# - Data: Male in Sheet3, female in Sheet4
# - Output variables:
#      - qxm_servRet_male
#      - qxm_servRet_female


get_mort1 <- function(data_sheet, data_range, rateName, file = file_path) {
  
	read_excel(file, sheet = data_sheet, range = data_range) %>% 
		select(age = 1, exposed = 3, death_assumed = 6) %>% 
  	mutate(!!rateName := death_assumed / exposed,
  				 age = str_extract(age, "\\d+") %>% as.numeric) %>% 
  	select(age, !!rateName)
} 


df_qxm_servRet <- 
		left_join(get_mort1("Sheet3", "c10:j70", "qxm_servRet_male"),
							get_mort1("Sheet4", "c10:j70", "qxm_servRet_female")
		) %>% 
  	mutate_all(funs(ifelse(is.nan(.), 0, .))) # convert NaNs to 0

df_qxm_servRet



## 2. Mortality for disabiltiy retirees ####

# - Assumed mortality rates are not provided.
# - Rates are calculated as "expected death under current assumption" (5) / "Total Exposure" (3)
# - Use 4-year period experience
# - Data: Male in Sheet10, female in Sheet11
# - Output variables:
#      - qxm_servRet_male
#      - qxm_servRet_female


df_qxm_disbRet <- 
	left_join(get_mort1("Sheet10", "c10:j70", "qxm_disbRet_male"),
						get_mort1("Sheet11", "c10:j70", "qxm_disbRet_female")
	)
df_qxm_disbRet



## 3. Mortality (ordinary) for active members ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet17, female in Sheet18
# - Output variables:
#      - qxm_actives_male
#      - qxm_actives_female

# Issue:
# - Assumed rates for age 70-74 are 0, but the expected death are actually calculated using rates at age 69. 
#   We use rates of 69 for age 70-74.


get_mort2 <- function(data_sheet, data_range, rateName, calc_rate = TRUE, indexVar = "age", file = file_path) {

 # data_sheet <- "Sheet17"
 # data_range <- "A10:J65"
 # file <- file_path
 # rateName <- "qxt_act_male"
 # calc_rate <- TRUE
			
	read_excel(file, sheet = data_sheet, range = data_range) %>% 
		select(Index = 1, exposed = 3, num_assumed = 7, rate_assumed = 5) %>% 
		mutate(calc_rate = calc_rate, # ifelse cannot take vector of conditions. If the condition is a single value while values are vectors, only the first element will be used. 
			     !!rateName  := ifelse(calc_rate, num_assumed / exposed, rate_assumed),
					 Index       = str_extract(Index , "\\d+") %>% as.numeric,
					 !!indexVar  := Index) %>% 
		select(!!indexVar, !!rateName)
} 


df_qxm_actives <-
	left_join(get_mort2("Sheet17", "A10:J65", "qxm_actives_male",   calc_rate = FALSE),
						get_mort2("Sheet18", "A10:J65", "qxm_actives_female", calc_rate = FALSE)
	)

# Adjust rates for 70-74
df_qxm_actives %<>% 
	mutate(qxm_actives_male   = ifelse(age < 70, qxm_actives_male,   qxm_actives_male[age == 69]),
				 qxm_actives_female = ifelse(age < 70, qxm_actives_female, qxm_actives_female[age == 69]))

df_qxm_actives



## 4. Retirement rate in the first year of eligibility  ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet24, female in Sheet25
# - Output variables:
#      - qxr_y1_male_EM    (elected+mandated)
#      - qxr_y1_female_EM  (elected+mandated)
 

df_qxr_y1_EM <-
	left_join(get_mort2("Sheet24", "A10:J42", "qxr_y1_male_EM",   calc_rate = FALSE),
						get_mort2("Sheet25", "A10:J42", "qxr_y1_female_EM", calc_rate = FALSE)
	)
df_qxr_y1_EM


df_qxr_y1_E <-
	left_join(get_mort2("Sheet43", "A10:J42", "qxr_y1_male_E",   calc_rate = FALSE),
						get_mort2("Sheet44", "A10:J42", "qxr_y1_female_E", calc_rate = FALSE)
	)
df_qxr_y1_E


df_qxr_y1_M <-
	left_join(get_mort2("Sheet61", "A10:J42", "qxr_y1_male_M",   calc_rate = FALSE),
						get_mort2("Sheet62", "A10:J42", "qxr_y1_female_M", calc_rate = FALSE)
	)
df_qxr_y1_M

df_qxr_y1 <- df_qxr_y1_EM %>% 
	 left_join(df_qxr_y1_E) %>% 
	 left_join(df_qxr_y1_M)

df_qxr_y1




## 5. Retirement rate in the second year of eligibility ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet27, female in Sheet28
# - Output variables:
#      - qxr_y2_male_EM
#      - qxr_y2_female_EM


df_qxr_y2_EM <-
	left_join(get_mort2("Sheet27", "A10:J42", "qxr_y2_male_EM",   calc_rate = FALSE),
						get_mort2("Sheet28", "A10:J42", "qxr_y2_female_EM", calc_rate = FALSE)
	)
df_qxr_y2_EM


df_qxr_y2_E <-
	left_join(get_mort2("Sheet46", "A10:J42", "qxr_y2_male_E",   calc_rate = FALSE),
						get_mort2("Sheet47", "A10:J42", "qxr_y2_female_E", calc_rate = FALSE)
	)
df_qxr_y2_E

df_qxr_y2_M <-
	left_join(get_mort2("Sheet64", "A10:J42", "qxr_y2_male_M",   calc_rate = FALSE),
						get_mort2("Sheet65", "A10:J42", "qxr_y2_female_M", calc_rate = FALSE)
	)
df_qxr_y2_M


df_qxr_y2 <- df_qxr_y2_EM %>% 
	 left_join(df_qxr_y2_E) %>% 
	 left_join(df_qxr_y2_M)

df_qxr_y2


## 6. Retirement rate AFTER the second year of eligibility ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet30, female in Sheet31
# - Output variables:
#      - qxr_y2post_male_EM
#      - qxr_y2post_female_EM


df_qxr_y2post_EM <-
	left_join(get_mort2("Sheet30", "A10:J42", "qxr_y2post_male_EM",   calc_rate = FALSE),
						get_mort2("Sheet31", "A10:J42", "qxr_y2post_female_EM", calc_rate = FALSE)
	)
df_qxr_y2post_EM


df_qxr_y2post_E <-
	left_join(get_mort2("Sheet49", "A10:J42", "qxr_y2post_male_E",   calc_rate = FALSE),
						get_mort2("Sheet50", "A10:J42", "qxr_y2post_female_E", calc_rate = FALSE)
	)
df_qxr_y2post_E


df_qxr_y2post_M <-
	left_join(get_mort2("Sheet67", "A10:J42", "qxr_y2post_male_M",   calc_rate = FALSE),
						get_mort2("Sheet68", "A10:J42", "qxr_y2post_female_M", calc_rate = FALSE)
	)
df_qxr_y2post_M



df_qxr_y2post <- df_qxr_y2post_EM %>% 
	left_join(df_qxr_y2post_E) %>% 
	left_join(df_qxr_y2post_M)

df_qxr_y2post


## 7. Retirement rates, early retirement ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet79, female in Sheet80 (same rate for male and female)
# - Output variables:
#      - qxr_early_male
#      - qxr_early_female


df_qxr_early <-
	left_join(get_mort2("Sheet79", "A10:J42", "qxr_early_male",   calc_rate = FALSE),
						get_mort2("Sheet80", "A10:J42", "qxr_early_female", calc_rate = FALSE)
	)
df_qxr_early



## 8. Withdrawal rates for active members ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet79, female in Sheet80 (same rate for male and female)
# - Output variables:
#      - qxr_early_male
#      - qxr_early_female


df_qxt <-
	left_join(get_mort2("Sheet87", "A10:J42", "qxt_male",   calc_rate = FALSE, indexVar = "yos"),
						get_mort2("Sheet87", "A10:J42", "qxt_female", calc_rate = FALSE, indexVar = "yos")
	)
df_qxt




## 9. Ordinary disability rates ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet93, female in Sheet94
# - Output variables:
#      - qxd_ord_male
#      - qxd_ord_female


df_qxd_ord <-
	left_join(get_mort2("Sheet93", "A10:J65", "qxd_ord_male",   calc_rate = FALSE),
						get_mort2("Sheet94", "A10:J65", "qxd_ord_female", calc_rate = FALSE)
	)
df_qxd_ord


## 10. Accidental disability rates ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use Assumed probability under "current assumption" 
# - Use 4-year period experience
# - Data: Male in Sheet100, female in Sheet101
# - Output variables:
#      - qxd_acc_male
#      - qxd_acc_female


df_qxd_acc <-
	left_join(get_mort2("Sheet100", "A10:J65", "qxd_acc_male",   calc_rate = FALSE),
						get_mort2("Sheet101", "A10:J65", "qxd_acc_female", calc_rate = FALSE)
	)
df_qxd_acc

df_qxd <- left_join(df_qxd_ord, df_qxd_acc)
df_qxd



## 11. Salary scales, total and merit ####

# - We consider the assumed probabilities provided are accurate (not rounded). 
# - Use "annual rates of salary increase"
# - Use 4-year period experience
# - Data: total in Sheet106, merit in Sheet111
# - Output variables:
#      - salScale_total
#      - salScale_merit

df_salScale <- 
	left_join(
		read_excel(file_path, sheet = "Sheet106", range = "A11:I42") %>% 
	      select(yos = 1, salScale_total = 5) %>% 
	      mutate(yos = str_extract(yos, "\\d+") %>% as.numeric),
    
    read_excel(file_path, sheet = "Sheet111", range = "A10:I41") %>% 
    	  select(yos = 1, salScale_merit = 5) %>% 
    	  mutate(yos = str_extract(yos, "\\d+") %>% as.numeric)
	)

df_salScale %>% 
	mutate(dif = salScale_total - salScale_merit)



#*************************************************************************************************************
#                               12. Loading MP2015 data                       #####                  
#*************************************************************************************************************

# Import projection scale (scale BB-2D)

data_scale_M <- read_excel(file_path, sheet = "MP2015_Male", skip = 1) %>% 
	filter(!is.na(Age)) %>% 
	mutate(Age = 20:120, 
				 gender = "male")
names(data_scale_M) <- c("age",1951:2030,"gender")


data_scale_F <- read_excel(file_path, sheet = "MP2015_Female", skip = 1) %>%
	filter(!is.na(Age)) %>% 
	mutate(Age = 20:120, 
				 gender = "female")
names(data_scale_F) <- c("age",1951:2030, "gender")



# Transform data to long format
data_scale_M %<>% gather(year_match, scale.M, -age, -gender) %>% mutate(year_match = as.numeric((year_match)))
data_scale_F %<>% gather(year_match, scale.F, -age, -gender) %>% mutate(year_match = as.numeric((year_match)))



# Expand the scales to 1915-2164
# 1915: the year when a 120-year old retiree in 2015 was at age 20. 
# 2235: the year when a 20-year old new entrant in 2115 will be at age 120.
# The scale BB-2D covers year 1951-2030. Years before 1951 use the scale for 1951, and years after 2030 use the scale for 2030. 


## Review and save results ####

df_qxm_servRet
df_qxm_disbRet
df_qxm_actives

df_qxr_y1
df_qxr_y2
df_qxr_y2post
df_qxr_early
df_qxd

df_qxt
df_salScale

data_scale_M
data_scale_F

save(df_qxm_servRet,
		 df_qxm_disbRet,
		 df_qxm_actives,
		 
		 df_qxr_y1,
		 df_qxr_y2,
		 df_qxr_y2post,
		 df_qxr_early,
		 df_qxd,
		 
		 df_qxt,
		 df_salScale,
		 
		 data_scale_M,
		 data_scale_F,
		 
		 file = paste0(dir_data, "Data_ES2015.RData")
		 )

df_qxt





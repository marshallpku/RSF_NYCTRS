#' This script loads decrement tables from the document
#' "Proposed Changes in Actuarial Assumpitons and Methods Used in Determining Employer Contribuitons
#' for Fiscal Years Beginning on and after July 1, 2018 for NYCTRS"


# General Comments

#' 1. The current prabilities in this documents are not always equal to the probabilities 
#'    given in the 2015 experience study. From the Nov 2018 draft, we will use probabilities
#'    from this document.  




# Notes on retirement rates of elected and mandated members
#   -  Upto age X, the combined rates are heavily weighted toward elected members
#   -  At ate X + 1, the combined rates are generally less weighted toward elected members (or even toward mandated members)
#   -  After age X + 1, the combined rates are the same as the mandated rates.
#   -  X = 
#          60 for 1st year eligibility
#          61 for 2nd year eligibility
#          62 for after 2nd year eligibility


quiet <- function(x) { 
	sink(tempfile()) 
	on.exit(sink()) 
	invisible(force(x)) 
} 


#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/"
file_name <- "NYCTRS_Decrements&SalaryScale_update2019.xlsx" 
file_path <- paste0(dir_data, file_name)
file_path



## 1. Mortality for Service Retirees ####

# - Data: Male in Sheet16, female in Sheet17
# - Output variables: _c for current, _p for proposed for FY2019 and after
#      - qxm_servRet_male_c 
#      - qxm_servRet_female_c
#      - qxm_servRet_male_p 
#      - qxm_servRet_female_p


get_mortRet <- function(data_sheet, data_range, rateName, file = file_path) {
	# Extract mortality rate for retirees (service and disability) and beneficiaries 
	
	rateName_c <- paste0(rateName, '_c')
	rateName_p <- paste0(rateName, '_p')
	
	df <- read_excel(file, sheet = data_sheet, range = data_range)
	df <- bind_rows(df[, 1:3]  %>% rename(age = 1, !!rateName_c := 2, !!rateName_p := 3 ), 
									df[, 4:6]  %>% rename(age = 1, !!rateName_c := 2, !!rateName_p := 3 )) %>% 
		filter(!is.na(age)) %>% 
		mutate_all(~ as.numeric(.))
}
	
# file <- file_path
# data_sheet <- 'Sheet16'
# data_range <- 'b2:g55'
# rateName <- 'qxm_servRet_male'

df_qxm_servRet <- left_join(get_mortRet("Sheet16", "b2:g55", "qxm_servRet_male"),
														get_mortRet("Sheet17", "b2:g55", "qxm_servRet_female")
														)
df_qxm_servRet



## 2. Mortality for disabiltiy retirees ####

# - Data: Male in Sheet18, female in Sheet19
# - Output variables:
#      - qxm_servRet_male_c
#      - qxm_servRet_female_c
#      - qxm_servRet_male_p
#      - qxm_servRet_female_p


df_qxm_disbRet <- 
	left_join(get_mortRet("Sheet18", "b2:G55", "qxm_disbRet_male"),
						get_mortRet("Sheet19", "b2:G55", "qxm_disbRet_female")
	)
df_qxm_disbRet



## 3. Mortality (ordinary) for active members ####

# - Data: in Sheet10
# - Output variables:
#      - qxm_actives_male_c
#      - qxm_actives_female_c
#      - qxm_actives_male_p
#      - qxm_actives_female_p

df_qxm_actives <- read_excel(file_path, sheet = 'Sheet10', range = 'b3:f69') %>% 
	filter(!is.na(Age)) %>% 
	mutate_all(~ as.numeric(.))

names(df_qxm_actives) <- c('age', 'qxm_actives_male_c', 'qxm_actives_female_c', 'qxm_actives_male_p', 'qxm_actives_female_p')
df_qxm_actives





## 4. Retirement rate in the first year of eligibility  ####

# - Data: in Sheet13 and Sheet14, same for both genders
# - Output variables:
#      - qxr_y1_M_c   
#      - qxr_y2_M_c   
#      - qxr_y2post_M_c   

#      - qxr_y1_E_c   
#      - qxr_y2_E_c   
#      - qxr_y2post_E_c   

#      - qxr_y1_M_t4_p  
#      - qxr_y1_M_t6_p  
#      - qxr_y1post_M_p 

#      - qxr_y1_E_p  
#      - qxr_y1post_E_p   

 
df_qxr_M <- read_excel(file_path, sheet = 'Sheet13', range = 'a3:g29') %>% 
	filter(!is.na(Age)) %>% 
	mutate_all(~ as.numeric(.))

names(df_qxr_M) <-  c(
	'age',
	'qxr_y1_M_c',
	'qxr_y2_M_c',   
	'qxr_y2post_M_c', 
	'qxr_y1_M_t4_p',  
	'qxr_y1_M_t6_p',  
	'qxr_y1post_M_p' 
)


df_qxr_E <- read_excel(file_path, sheet = 'Sheet14', range = 'a3:f29') %>% 
	filter(!is.na(Age)) %>% 
	mutate_all(~ as.numeric(.))

names(df_qxr_E) <-  c(
	'age',
	'qxr_y1_E_c',
	'qxr_y2_E_c',   
	'qxr_y2post_E_c', 
	'qxr_y1_E_p',  
	'qxr_y1post_E_p' 
)

df_qxr_E





## 5. Retirement rates, early retirement ####

# - Data: in Sheet15 (same rate for male and female)
# - Output variables:
#      - qxr_early_c
#      - qxr_early_t4_p
#      - qxr_early_t6_p

df_qxr_early <- read_excel(file_path, sheet = 'Sheet15', range = 'a2:d28') %>% 
	filter(!is.na(Age)) %>% 
	mutate_all(~ as.numeric(.))
names(df_qxr_early) <- c("age", 'qxr_early_c', 'qxr_early_t4_p', 'qxr_early_t6_p')

df_qxr_early


## 6. Withdrawal rates for active members ####

# - Data: in Sheet9 (same rate for male and female)
# - Output variables:
#      - qxt_c
#      - qxt_p

df_qxt <- read_excel(file_path, sheet = 'Sheet9', range = 'b2:c28') %>% 
	mutate_all(~ as.numeric(.)) %>% 
	rename(yos = 1, 
				 qxt_c = 2) %>% 
	mutate(qxt_p = qxt_c)

df_qxt



## 7. Ordinary disability rates ####
# - Data: Male in Sheet 11
# - Output variables:
#      - qxd_ord_male_c
#      - qxd_ord_female_c
#      - qxd_ord_male_p
#      - qxd_ord_female_p

df_qxd_ord <- read_excel(file_path, sheet = 'Sheet11', range = 'a7:e73') %>% 
	mutate_all(~ as.numeric(.)) %>% 
	rename(age = 1,
				 qxd_ord_male_c = 2,
				 qxd_ord_female_c = 3,
				 qxd_ord_male_p = 4,
				 qxd_ord_female_p = 5)

df_qxd_ord


## 8. Accidental disability rates ####


# - Data: Male in Sheet100, female in Sheet101
# - Output variables:
#      - qxd_acc_male_c
#      - qxd_acc_female_c
#      - qxd_acc_male_p
#      - qxd_acc_female_p

df_qxd_acc <- read_excel(file_path, sheet = 'Sheet12', range = 'a6:e72') %>% 
	mutate_all(~ as.numeric(.)) %>% 
	rename(age = 1,
				 qxd_acc_male_c = 2,
				 qxd_acc_female_c = 3,
				 qxd_acc_male_p = 4,
				 qxd_acc_female_p = 5)

df_qxd_acc



## 9. Salary scales, total and merit ####

# - Data: in Sheet22
# - Output variables:
#      - salScale_total_c
#      - salScale_merit_c
#      - salScale_total_p
#      - salScale_merit_p


df_salScale <- read_excel(file_path, sheet = 'Sheet22', range = 'a3:c26') %>% 
	mutate_all(~ as.numeric(.)) %>% 
	rename(yos = 1,
				 salScale_merit_c = 2,
				 salScale_total_c = 3) %>% 
	mutate(salScale_merit_p = salScale_merit_c,
				 salScale_total_p = salScale_total_c)
df_salScale


#*************************************************************************************************************
#                               10. Loading MP2018 data                       #####                  
#*************************************************************************************************************

# Import projection scale (scale BB-2D)

data_scale_M <- read_excel(file_path, sheet = "MP2018_Male", skip = 1) %>% 
	mutate(gender  = "male",
				 MP_table = "MP2018")
names(data_scale_M) <- c("age",1951:2034,"gender", 'MP_table')
data_scale_M

data_scale_F <- read_excel(file_path, sheet = "MP2018_Female", skip = 1) %>% 
	mutate(gender = "female",
				 MP_table = "MP2018")
names(data_scale_F) <- c("age",1951:2034,"gender", 'MP_table')
data_scale_F

# Transform data to long format
data_scale_M %<>% gather(year_match, scale.M, -age, -gender, -MP_table) %>% mutate(year_match = as.numeric((year_match)))
data_scale_F %<>% gather(year_match, scale.F, -age, -gender, -MP_table) %>% mutate(year_match = as.numeric((year_match)))



# Expand the scales to 1915-2164
# 1915: the year when a 120-year old retiree in 2015 was at age 20. 
# 2235: the year when a 20-year old new entrant in 2115 will be at age 120.
# The scale BB-2D covers year 1951-2030. Years before 1951 use the scale for 1951, and years after 2030 use the scale for 2030. 


## Review and save results ####

df_qxm_servRet
df_qxm_disbRet
df_qxm_actives

df_qxr_M
df_qxr_E
df_qxr_early
df_qxd_acc
df_qxd_ord

df_qxt
df_salScale

data_scale_M
data_scale_F

save(
	df_qxm_servRet,
	df_qxm_disbRet,
	df_qxm_actives,
	
	df_qxr_M,
	df_qxr_E,
	df_qxr_early,
	df_qxd_acc,
	df_qxd_ord,
	
	df_qxt,
	df_salScale,
	
	data_scale_M,
	data_scale_F,
		 
	file = paste0(dir_data, "Data_update2018.RData")
	)






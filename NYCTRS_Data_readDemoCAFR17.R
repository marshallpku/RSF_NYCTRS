#This script loads demographic data provided in the 2017 CAFR of NYCTRS. 


# Tables to load:
  # Active members by age  (as of Jun30, 2016)  : p165 (Sheet4) Schedule 6 Table of average salaries of in-service members-QPP 
  # Active members by tier (2007-2017)          : p166 (Sheet5) Schedule 7 In-service membership by tier and by title - QPP
  # Service retirees by age (as of Jun30, 2016) : p169 (Sheet8) Schedule 13 service retirement allowance - QPP
  # Disability retirees by age (as of Jun30, 2016) :p170 (Sheet9) Schedule 14 and 15 Ordinary/Accident disability retirement allowance - QPP
  # Survivors by age (as of Jun30, 2016)        : p171 (Sheet10 ) Schedule 16 Survivors' benefit - QPP      
  # TDA membership by age (as of Jun30 2017)    : p174 (Sheet14) Schedule 23 Membership by age and type (count and fund balance )


# Tables that are not loaded but are useful modeling and/or calibration
  # Average years of service by gender p167 (Sheet6)
  # Payment options chosen at Retirement p167 (Sheet6), also average age at retirement
  # Retirees' average monthly payments and FAS by YOS p168 (Sheet7) (For calibration?)
  # TDA program summary (changes in membership): schedule 21 (Sheet12) (increased from 70k to 85k in 10 years)
  # TDA annuitant summary: schedule 22 (Sheet13) (number decreasing over time)
  # TDA withdrawal by age and type (count and amount) Schedule 24 (Sheet13): RMD,Partial, survivors,  payments, total, 401a service purchase 
  # TDA fund conversion: schedule 25 (Sheet14): most common conversion: VA(diversified equity fund) to FX (Fixed Return fund), 
  #                                             VE (Socially Responsive Equity fund) to FX common among young members. 




## Setting file path
dir_data  <- "Inputs_data/"
file_name <- "RawMaterials/2017 CAFR Statistics converted by Nuance.xlsx" 
file_path <- paste0(dir_data, file_name)
file_path



## 1. Active members ####

df_nactives <- 
read_excel(file_path, sheet = "Sheet4", range = "A22:F33") %>% 
	select(age_grp = 1, nactives_male = 2, salary_male = 3, nactives_female = 5, salary_female = 6) %>% 
	separate(age_grp, c("age_lb", "age_ub"), convert= T) %>%
	mutate_at(vars(age_lb, age_ub), funs(as.numeric(.))) 

df_nactives[1,                 c("age_lb", "age_ub")] <- c(20, 24)
df_nactives[nrow(df_nactives), c("age_lb", "age_ub")] <- c(70, 74)	
df_nactives



## 2. share of tiers ####

df_TierShares <- 
	read_excel(file_path, sheet = "Sheet5", range = "A7:G16", 
						 col_names = c("year", "age_avg", "Teir1", "Tier2", "Tier3", "Tier4", "Tier6"))



## 3. Service retirees ####

df_nservRet <- 
	read_excel(file_path, sheet = "Sheet8", range = "A16:F29") %>% 
	select(age_grp = 1, nservRet_male = 2, benefit_male = 3, nservRet_female = 5, benefit_female = 6) %>% 
	separate(age_grp, c("age_lb", "age_ub")) %>%
	mutate_at(vars(age_lb, age_ub), funs(as.numeric(.))) 
df_nservRet[nrow(df_nservRet), "age_ub"] <- 94

df_nservRet


## 4. Disability retirees ####

df_ndisbRet_ord <- 
	read_excel(file_path, sheet = "Sheet9", range = "A12:F26") %>% 
	select(age_grp = 1, ndisbRet_ord_male = 2, benefit_male = 3, ndisbRet_ord_female = 5, benefit_female = 6) %>% 
	separate(age_grp, c("age_lb", "age_ub")) %>%
	mutate_at(vars(age_lb, age_ub, benefit_male, benefit_female), funs(na2zero(as.numeric(.)))) 


df_ndisbRet_ord[1, c("age_lb", "age_ub") ] <- c(25,29)
df_ndisbRet_ord[nrow(df_ndisbRet_ord), c("age_lb", "age_ub")] <- c(90, 94)
df_ndisbRet_ord


df_ndisbRet_acc <- 
	read_excel(file_path, sheet = "Sheet9", range = "A36:F50") %>% 
	select(age_grp = 1, ndisbRet_acc_male = 2, benefit_male = 3, ndisbRet_acc_female = 5, benefit_female = 6) %>% 
	separate(age_grp, c("age_lb", "age_ub")) %>%
	mutate_at(vars(age_lb, age_ub, benefit_male, benefit_female), funs(na2zero(as.numeric(.)))) 


df_ndisbRet_acc[1, c("age_lb", "age_ub") ] <- c(25,29)
df_ndisbRet_acc[nrow(df_ndisbRet_acc), c("age_lb", "age_ub")] <- c(90, 94)
df_ndisbRet_acc


## 5. Survivors ####

df_nsurvivors <- 
	read_excel(file_path, sheet = "Sheet10", range = "A7:F21") %>% 
	select(age_grp = 1, nsurvivors_male = 2, benefit_male = 3, nsurvivors_female = 5, benefit_female = 6) %>% 
	separate(age_grp, c("age_lb", "age_ub")) %>%
	mutate_at(vars(age_lb, age_ub), funs(as.numeric(.))) 


df_nsurvivors[1, c("age_lb", "age_ub") ] <- c(25,29)
df_nsurvivors[nrow(df_ndisbRet_ord), c("age_lb", "age_ub")] <- c(90, 94)
df_nsurvivors


## 6. TDA withdrawals ####

df_TDAwithdrawal <- 
    read_excel(file_path, sheet = "Sheet14", range = "A7:K20",
    					 col_names = c("age",
    					 							 "n_partial", "d_partial",
    					 							 "n_401k",    "d_401k",
    					 							 "n_RMD",     "d_RMD",
    					 							 "n_total",   "d_total",
    					 							 "n_surv",    "d_surv")) %>% 
	  mutate(age = str_extract(age, "\\d+")) %>% 
	  mutate_all(funs(na2zero(as.numeric(.)) ))

df_TDAwithdrawal



## Review and save results ####

df_nactives
df_ndisbRet_acc
df_ndisbRet_ord
df_nsurvivors
df_TierShares
df_TDAwithdrawal

save(df_nactives,
		 df_ndisbRet_acc,
		 df_ndisbRet_ord,
		 df_nsurvivors,
		 df_TierShares,
		 df_TDAwithdrawal,
		 file = paste0(dir_data, "Data_DemoCAFR17.RData")
)





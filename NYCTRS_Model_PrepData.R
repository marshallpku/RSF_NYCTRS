# This script prepares salary data and initital population data for modeling


# Road Map

# 1. Salary scale
  # get.salgrowth
  # get_scale
  # fill_startSal
  # get_salary
  # get_salary_proc: Top level function that runs the four functions above
  
# 2. Benefit for initial retirees 
  # get_benefit_servRet
  # get_benefit_disbRet
  # get_benefit_survivors

# 3. Initial population
  # get_initPop 

# 4. Distribution of new entrants
  # get_entrantsDist

# 5. functions for get results for a given tier
  # get_benefit_tier
  # get_initPop_tier
  # get_entrantsDist_tier



# Summary of outputs

# salary
# benefit
# init_pop
# entrants_dist

# 

# get_tierData <- function(df, tier, grouping = paramlist$Grouping) df %<>% filter(grepl(tier, planname), grepl(grouping, planname))

#get_tierData(init_actives_all, "tF") %>% print


#*************************************************************************************************************
#                                Global settings                       #####                  
#*************************************************************************************************************
# parameters needed for function
Tier_select <- "t4a"
dir_data <- "Inputs_data/"


#*************************************************************************************************************
#                                Loading data                       #####                  
#*************************************************************************************************************
load(paste0(dir_data, "Data_initDemographics_CAFR17.RData"))
load(paste0(dir_data, "Data_ES2015.RData"))


#*************************************************************************************************************
#                         Salary 1.  Create salary scale                   #####                  
#*************************************************************************************************************

df_salScale
# pretreatment of df_salScale 
get_salgrowth <- function() {
  max_yos <- with(Global_paramlist, max_retAge - min_ea)
  max_yos_data <- df_salScale$yos[nrow(df_salScale)]
  
  data.frame(yos = 0:max_yos) %>%
    left_join(df_salScale) %>%
    mutate(salgrowth = ifelse(yos > max_yos_data, salScale_total[yos == max_yos_data], salScale_total)) %>%
    select(yos, salgrowth)
}

# salgrowth <- get_salgrowth()
# salgrowth

#*************************************************************************************************************
#                         Salary 2.  Create complete salary scale                                    #####                  
#*************************************************************************************************************

get_scale <- function(
   salgrowth_ = salgrowth,
   paramlist_ = paramlist,
   Global_paramlist_  = Global_paramlist
  ){
  
  # This function generates a complete salary scale for all combos of starting year, entry ages and ages relevant to
  # to model. 
  # 
  # Salary levels at year 1 are set to 1. For future workers (entry year greater than 1) whose span of career years
  # do not include year 1, assumption about their starting salary levels is needed. Curretnly we assume starting salary
  # grows at inflation rate. 
  
  
  # Run the section below when developing new features. 
  # salgrowth_ = salgrowth
  # paramlist_ = paramlist
  # Global_paramlist_ = Global_paramlist
  
  
   assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
   assign_parmsList(paramlist_,        envir = environment())
  # 
  

  
  #.salgrowth <- .salgrowth %>% select(yos, salgrowth_w) # %>% filter(planname == planname_sscale) %>% select(-planname)
  
  SS.all <- expand.grid(start_year = (1 - (max_age - min_age)):nyear, # smallest relevant start year is the entry year who is at the max_age in year 1
                        ea = range_ea, 
                        age = min_age:(max_retAge - 1)) %>% 
    filter(age >= ea, start_year + (max_retAge - 1 - ea) >= 1 ) %>% # workers must stay in workforce at least up to year 1. 
    mutate(yos = age - ea) %>% 
    left_join(salgrowth_) %>%
    group_by(start_year, ea) %>% 
    mutate(year = start_year + (age - ea),
           growth_start = (1 + startingSalgrowth)^(start_year - 1),  # assume starting salary grows at the rate of inflation for all entry ages 
           scale = cumprod(ifelse(age == ea, 1, lag(1 + salgrowth))), # salgrowth is from data salgrowth
           scale = ifelse(start_year <= 1, scale/scale[year == 1],    # Salary levels before starting year are scaled based on salary in the initial year.
                          scale * growth_start)) %>% 
    ungroup %>% 
    mutate(year       = init_year + year - 1,
           start_year = init_year + start_year - 1 # convert to fiscal year. 
    ) %>% 
    select(start_year, ea, age, year, scale) %>% 
    arrange(start_year, ea, age)
  
  return(SS.all)
}

# SS.all <- get_scale()

#*************************************************************************************************************
#                         Salary 3. Supplement the inital salary table with all starting salary        #####                  
#*************************************************************************************************************

fill_startSal <- function(actives_,         # = tailored_demoData$actives,
                          paramlist_        = paramlist,
                          Global_paramlist_ = Global_paramlist
                          ){
  
  # This function generates a table of initial salary (year 1) which include all starting salary levels (age = ea)
  # If the starting salary is missing from the actives data frame, spline function is used to interpolate and/or 
  # extraploate the missing values. 
  
  # Run the section below when developing new features.
    # actives_          = init_actives %>% select(age, yos, ea, salary)
    # paramlist_        = paramlist
    # Global_paramlist_ = Global_paramlist
  
   assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
   assign_parmsList(paramlist_,        envir = environment())  
   
  sal <- actives_ %>% select(age, ea, salary)

  sal.start <- splong(sal, "ea", range_ea) %>% filter(age == ea) %>% select(-age) %>% splong("ea", range_ea) %>% mutate(age = ea)
  
  sal <- rbind(sal, sal.start) 
  
  sal <- sal[!duplicated(sal[c("age","ea")]),]
  # sal %>% spread(age, salary)
  
  return(sal)
  
}

# get_tierData(init_actives_all, "t6") %>%  fill_startSal %>% ungroup %>% arrange(ea, age)

# init_sal <- fill_startSal(init_actives)


#*************************************************************************************************************
#                          Salary 4. Create complete salary history                                    #####                  
#*************************************************************************************************************

get_salary <- function(SS.all_   = SS.all,
                       init_sal_ = init_sal,
                       paramlist_= paramlist,
                       Global_paramlist_  = Global_paramlist
                       ){
  
  # Run the section below when developing new features.
    # SS.all_   = SS.all
    # init_sal_ =  init_sal
    # paramlist_= paramlist
    # Global_paramlist_  = Global_paramlist
  
   assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
   assign_parmsList(paramlist_,        envir = environment())  
  
  salary <- 
    SS.all_ %>% 
    left_join(init_sal_) %>% 
    group_by(start_year, ea) %>% 
    mutate(sx = ifelse(start_year <= init_year, salary[year == init_year] * scale, 
                       salary[age == ea]* scale)) %>% 
    select(start_year, ea, age, year, sx)
  
  return(salary)
}

# salary <- get_salary() 
# salary %>% filter(start.year < 2015) %>% arrange(start.year, ea, age)



#*************************************************************************************************************
#                          Salary 5.  The top level function for creating initial salary              #####                  
#*************************************************************************************************************

get_salary_proc <- function(){ 

# Inputs
 # Tier_select
 # salgrowth
 # w.salgrwoth.method
 # init_actives 
 # paramlist
 # Global_paramlist

salgrowth.fn <- get_salgrowth()

SS.all.fn    <- get_scale(salgrowth.fn)

init_sal.fn  <- fill_startSal(init_actives)

salary.fn <- get_salary(SS.all.fn, init_sal.fn) %>% 
  mutate(sx = na2zero(sx))

return(salary.fn)
}

# salary <- get_salary_proc()

# x <-  get_salary_proc("tCD") %>% arrange(start.year, ea, age)



#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************
get_benefit_servRet <- function(
   servRet_,
   paramlist_ = paramlist,
   Global_paramlist_  = Global_paramlist
  ){
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())  
  
  benefit <- 
    servRet_ %>% 
    select(age, benefit_servRet) %>% 
    mutate(year       = init_year,
           ea         = min_age,
           age_servRet= age,
           start_year = year - (age - ea)) %>% 
    select(start_year, ea, age, age_servRet, benefit_servRet)
  # benefit %>% select(-year) %>% spread(age, benefit)
  
  return(benefit)
}


get_benefit_disbRet <- function(
  disbRet_, 
  paramlist_ = paramlist,
  Global_paramlist_  = Global_paramlist
){
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())  
  
  benefit <-
    disbRet_ %>% 
    select(age, benefit_disbRet) %>%  
    mutate(year       = init_year,
           ea         = min_age,
           age_disbRet= age,
           start_year = year - (age - ea)) %>% 
    select(start_year, ea, age, age_disbRet, benefit_disbRet)

  return(benefit)
}


get_benefit_survivors <- function(
  survivors_, 
  paramlist_ = paramlist,
  Global_paramlist_  = Global_paramlist
){
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())  
  
  benefit <-
    survivors_ %>% 
    select(age, benefit_survivors) %>%  
    mutate(year       = init_year,
           ea         = min_age,
           # age_disb   = age,
           start_year = year - (age - ea)) %>% 
    select(start_year, ea, age, benefit_survivors)
  
  return(benefit)
}


# benefit_servRet <- get_benefit_servRet(init_servRet)
# benefit_disbRet <- get_benefit_disbRet(init_disbRet)
# benefit_survivors <- get_benefit_survivors(init_survivors)
# 
# benefit_servRet
# benefit_disbRet
# benefit_survivors

# get_tierData(init_retirees_all, "tCD") %>% get_benefit()
# get_tierData(init_disb_all, "tCD") %>% get_benefit.disb()



#*************************************************************************************************************
#                               Generating inital population                                             #####                  
#*************************************************************************************************************




get_initPop <- function (init_actives_ = init_actives,       
                         init_servRet_ = init_servRet,
                         init_disbRet_ = init_disbRet,
                         init_survivors_ = init_survivors,
                         init_terms_   = init_terms,
                         paramlist_        = paramlist,
                         Global_paramlist_ = Global_paramlist,
                         trim = TRUE
                         ){
  # Import and standardize the total number of actives and retirees.  
  
  # Run the section below when developing new features.
  # init_actives_    = init_actives       #get_tierData(init_actives_all, Tier_select_)
  # init_servRet_    = init_servRet       #get_tierData(init_retirees_all, Tier_select_)
  # init_disbRet_    = init_disbRet  
  # init_survivors_  = init_survivors
  # init_terms_      = init_terms
  # trim = TRUE
  # paramlist_        = paramlist
  # Global_paramlist_ = Global_paramlist
  
  
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())
  
  
  # Initial actives
  init_actives_ %<>% select(ea, age, nactives)
    
  if(trim) init_actives_ %<>% filter(ea %in% range_ea, age %in% min_age:(max_retAge - 1))
  
  init_actives_ <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_actives_) 
  init_actives_ %<>% spread(age, nactives, fill = 0) %>% select(-ea) %>% as.matrix 
  
  
  # Initial service retirees
  init_servRet_ <- init_servRet_ %>% select(age, nservRet) %>% mutate(ea = min_age) 
  init_servRet_ <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_servRet_) %>% 
    spread(age, nservRet, fill = 0) %>% select(-ea) %>% as.matrix
  
  # Initial survivors
  init_survivors_ <- init_survivors_ %>% select(age, nsurvivors) %>% mutate(ea = min_age) 
  init_survivors_ <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_survivors_) %>% 
    spread(age, nsurvivors, fill = 0) %>% select(-ea) %>% as.matrix
  
  # Intitial vested terminated workers 
  init_terms_ <- init_terms_ %>% select(age, nterms) %>% mutate(ea = min_age) 
  init_terms_ <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_terms_) %>% 
    spread(age, nterms, fill = 0) %>% select(-ea) %>% as.matrix
  
  # Initial disability retirees
  init_disbRet_ <- init_disbRet_ %>% select(age, ndisbRet) %>% mutate(ea = min_age) 
  init_disbRet_ <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_disbRet_) %>% 
    spread(age, ndisbRet, fill = 0) %>% select(-ea) %>% as.matrix
  
  # # Initial terminated vested 
  # init_terms <- .terminated %>% select(ea, age, nterms)
  # init_terms <-  expand.grid(ea = range_ea, age = range_age) %>% left_join(init_terms) %>% 
  #   #mutate(nactives = n_init_actives * nactives/sum(nactives, na.rm = TRUE)) %>%
  #   spread(age, nterms, fill = 0) %>% select(-ea) %>% as.matrix 
  
  return(list(actives = init_actives_, 
              servRet = init_servRet_, 
              disbRet = init_disbRet_,
              survivors = init_survivors_,
              terms = init_terms_
              ))
}

# initPop <- get_initPop()
# x <- get_initPop(get_tierData(init_actives_all, Tier_select),
#             get_tierData(init_retirees.la_all, Tier_select),
#             get_tierData(init_terms_all, Tier_select),
#             get_tierData(init_disb.la_all, Tier_select)
#             )

#x$actives
# x$retirees %>% sum


#*************************************************************************************************************
#                            Infering ditribution of entrants from low yos actives                       #####                  
#*************************************************************************************************************


get_entrantsDist <- function(actives_,          
                             paramlist_        = paramlist,
                             Global_paramlist_ = Global_paramlist,
                             simple = FALSE){
  # Simple imputation rule is applied under the following circumstances:
  # 1. parameter "simple" is set to TRUE
  # 2. negative weights are generated by the regular rule. 
  
     # actives_          = init_actives # get_tierData(init_actives_all, Tier_select)
     # simple = F
     # paramlist_        = paramlist
     # Global_paramlist_ = Global_paramlist
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())   
  
  nact <- actives_ %>% select(age, ea, nactives)
  #nact %>% spread(age, nactives)
  
  # ## Distributon by simple rule
  # nact1 <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives)) %>% right_join(data.frame(ea = range_ea))
  # N <- 1
  # while(any(is.na(nact1$avg_ent))) {
  #   if(N <= length(nact1)) nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lag(avg_ent) , avg_ent)) else
  #     nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lead(avg_ent) , avg_ent))
  #   N <- N + 1
  #   }
  
  # For safty, do interpolation for potential missing cells.   
  nact <- splong(nact, "ea", range_ea) %>% splong("age", range_ea) %>% filter(age >= ea)
  nact %>% spread(age, nactives)
  
  # Estimate entrant distribution with low yos members
  ent <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives))
  
  neg_ea <- ent[which(ent$avg_ent < 0), "ea"]
  
  if(any(ent$avg_ent < 0)){warning("Negative inferred value(s) in the following entry age(s): " , as.character(neg_ea), "\n")
                                   #"  Simple imputation rule is applied")
    #ent <- nact1                          
  }
  
  # ent %<>% mutate(avg_ent = ifelse(avg_ent < 0, 0, avg_ent))
  
  # if(simple) ent <- nact1
  
  dist <- lowess(ent$avg_ent, f= 0.1)$y
  dist <- dist/sum(dist)
  
  return(dist)
}


# entrants_dist <- get_entrantsDist(init_actives)
# entrants_dist





# salary <- get_salary_proc()
# 
# benefit_servRet <- get_benefit_servRet(init_servRet)
# benefit_disbRet <- get_benefit_disbRet(init_disbRet)
# benefit_survivors <- get_benefit_survivors(init_survivors)
# 
# initPop <- get_initPop()
# 
# entrants_dist <- get_entrantsDist(init_actives)

# salary
# benefit_servRet
# benefit_disbRet
# benefit_survivors
# initPop
# entrants_dist


# #*************************************************************************************************************
# #                                       Functions to create tier specific data                    #####                  
# #*************************************************************************************************************
# 
# get_benefit_tier <- function(Tier_select_, grouping_ = paramlist$Grouping){
#   
#   # cola <- tier.param[Tier_select_, "cola"]
#   
#   #init_actives       <- get_tierData(init_actives_all, Tier_select_)
#   init_retirees      <- get_tierData(init_retirees.la_all, Tier_select_)
#   #init_beneficiaries <- get_tierData(init_beneficiaries_all, Tier_select_)
#   #init_terminated    <- init_terminated_all %>%  filter(grepl(Tier_select, planname, Tier_select_))
#   init_retirees %>% get_benefit() # %>% mutate(benefit = benefit * 0.989589)    # mutate(benefit = benefit * (1 + cola))
# 
# }
# 
# get_benefit.disb_tier <- function(Tier_select_, grouping_ = paramlist$Grouping){
#   
#   # cola <- tier.param[Tier_select_, "cola"]
#   init_disb      <- get_tierData(init_disb.la_all, Tier_select_)
#   init_disb %>% get_benefit.disb() # %>% mutate(benefit.disb = benefit.disb * 0.989589 )  # %>% mutate(benefit.disb = benefit.disb * (1 + cola))
#   
# }
# 
# 
# 
# get_initPop_tier <- function(Tier_select_, grouping_ =  paramlist$Grouping){
#   #Tier_select_ = Tier_select
#   #grouping_ =  paramlist$Grouping
#   
#   init_actives        <- get_tierData(init_actives_all, Tier_select_)
#   init_retirees       <- get_tierData(init_retirees.la_all, Tier_select_)
#   #init_beneficiaries <- get_tierData(init_beneficiaries_all, Tier_select_)
#   # init_terminated     <- init_terms_all %>%  filter(grepl(Tier_select_, planname))
#   init_terminated     <- get_tierData(init_terms_all, Tier_select_)
#   init_disb           <- get_tierData(init_disb.la_all,  Tier_select_)
#   
#   get_initPop(init_actives, init_retirees, init_terminated, init_disb)
#   
# }
# 
# 
# get_entrantsDist_tier <- function(Tier_select_, grouping_ =  paramlist$Grouping){
#   init_actives       <- get_tierData(init_actives_all, Tier_select_)
#   get_entrantsDist(init_actives)
# }


# get_benefit_tier("tE")
# get_initPop_tier("tE")
# get_entrantsDist_tier("tE")








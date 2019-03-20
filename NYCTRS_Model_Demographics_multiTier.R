# Simulation of the demograhics for multiple tiers of NYCTRS

## Modifications on the original model
  # 1. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
  # 2. The mortality for retirees are now retirement age dependent. (PSERS) ??


get_Population <- function(init_pop_t4a_         = init_pop_t4a,
                           decrement_model_t4a_  = decrement_model_t4a,
                           
													 init_pop_t4b_         = init_pop_t4b,
													 decrement_model_t4b_  = decrement_model_t4b,
													 
													 init_pop_t6_         = init_pop_t6,
													 decrement_model_t6_  = decrement_model_t6,
													 
													 entrants_dist_    = entrants_dist,
													 
													 paramlist_        = paramlist,
                           Global_paramlist_ = Global_paramlist){

## Inputs
# - range_ea:         all possible entry ages  
# - range_age:        range of age
# - nyear:            number of years in simulation
# - wf_growth:        growth rate of the size of workforce
# - no_entrance:      no new entrants into the workforce if set "TRUE". Overrides "wf_growth"
# - decrement.model:  Decrement table, from Model_Decrements.R  
# - Initial workforce for each type:
#    - init_pop$actives:   matrix, max ea by max age
#    - init_pop$retirees:  matrix, max ea by max age


## An array is created for each of the 6 status:
#  (1)Active     (dim = 3)
#  (2)Terminated (dim = 4)
#  (3)Retired    (dim = 4)
#  (4)Disabled   (dim = 4) life annuitants
#  (5)Dead       (dim = 3) We do not really need an array for dead, what's needed is only the total number of dead.  

# Run the section below when developing new features.   
	# init_pop_t4a_         = init_pop_t4a
	# decrement_model_t4a_  = decrement_model_t4a
	# 
	# init_pop_t4b_         = init_pop_t4b
	# decrement_model_t4b_  = decrement_model_t4b
	# 
	# init_pop_t6_         = init_pop_t6
	# decrement_model_t6_  = decrement_model_t6
	# 
	# entrants_dist_    = entrants_dist
	# 
	# paramlist_        = paramlist
	# Global_paramlist_ = Global_paramlist

 assign_parmsList(Global_paramlist_, envir = environment())
 assign_parmsList(paramlist_,        envir = environment())  


#*************************************************************************************************************
#                                     Creating arrays for each status ####
#*************************************************************************************************************

## In each 3D array, 
#  dimension 1(row) represents entry age, 
#  dimension 2(column) represents attained age,
#  dimension 3(depth) represents number of year, 
#  dimension 4, if applicable, represents the year of termination/retirement/disablity/death. 

# The array of actives has 3 dimensions: ea x age x year 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, 
                    range_age, 
                    init_year:(init_year + nyear - 1))

# The array of terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.term      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.term <- list(range_ea, 
                         range_age, 
                         init_year:(init_year + nyear - 1), 
                         (init_year) :(init_year + nyear - 1))


# The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.la <- list(range_ea, 
                       range_age, 
                       init_year:(init_year + nyear - 1), 
                       init_year:(init_year + nyear - 1))

# The array of death beneficiaries has 4 dimensions: ea x age x year x year of death(of the active)
wf_dim.deathBen      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.deathBen <- list(range_ea, 
                             range_age, 
														 init_year:(init_year + nyear - 1), 
                             init_year:(init_year + nyear - 1))

# The array of disability retirees has 4 dimensions: ea x age x year x year of disability
wf_dim.disb      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.disb <- list(range_ea, range_age, 
                         init_year:(init_year + nyear - 1), 
                         init_year:(init_year + nyear - 1))





wf_active_t4a <- wf_active_t4b <- wf_active_t6  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_dead_t4a <- wf_dead_t4b <- wf_dead_t6 <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_t4a <- wf_term_t4b <- wf_term_t6 <- array(0, wf_dim.term, dimnames = wf_dimnames.term)
wf_la_t4a   <- wf_la_t4b   <- wf_la_t6   <- array(0, wf_dim.la, dimnames = wf_dimnames.la)
wf_deathBen_t4a <- wf_deathBen_t4b <- wf_deathBen_t6 <- array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
wf_disbRet_t4a  <- wf_disbRet_t4b  <- wf_disbRet_t6  <- array(0, wf_dim.disb,     dimnames = wf_dimnames.disb)


newDeath.act_t4a  <- newDeath.act_t4b <- newDeath.act_t6 <- numeric(nyear)
newDeath.ret_t4a  <- newDeath.ret_t4b <- newDeath.ret_t6 <- numeric(nyear)
newDeath.term_t4a <- newDeath.term_t4b <- newDeath.term_t6 <- numeric(nyear)

newDisb.act_t4a <- newDisb.act_t4b <- newDisb.act_t6 <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting initial population  ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at age 54 and retireed in year 1. 
# Altough this may produce yos greater than r.max - ea.min, it is irrelevant to the calculation since we do not care about initial retirees' yos.  


# Tier 4 basic 
wf_active_t4a[, , 1]      <- init_pop_t4a_$actives 
wf_la_t4a[, , 1, 1]       <- init_pop_t4a_$servRet
wf_term_t4a[, , 1, 1]     <- init_pop_t4a_$terms   # note that the initial terms are assigned to year.term = init_year - 1
wf_disbRet_t4a[, , 1, 1]  <- init_pop_t4a_$disbRet

# Tier 4 55 program 
wf_active_t4b[, , 1]      <- init_pop_t4b_$actives 
wf_la_t4b[, , 1, 1]       <- init_pop_t4b_$servRet
wf_term_t4b[, , 1, 1]     <- init_pop_t4b_$terms   # note that the initial terms are assigned to year.term = init_year - 1
wf_disbRet_t4b[, , 1, 1]  <- init_pop_t4b_$disbRet

# Tier 6 
wf_active_t6[, , 1]      <- init_pop_t6_$actives 
wf_la_t6[, , 1, 1]       <- init_pop_t6_$servRet
wf_term_t6[, , 1, 1]     <- init_pop_t6_$terms   # note that the initial terms are assigned to year.term = init_year - 1
wf_disbRet_t6[, , 1, 1]  <- init_pop_t6_$disbRet



 


#*************************************************************************************************************
#                                     Defining population dynamics  ####
#*************************************************************************************************************

## Transition matrices ####
#   Assume the actual decrement rates are the same as the rates in decrement tables.
#   Later we may allow the actual decrement rates to differ from the assumed rates. 


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transtion matrix
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}


decrement_wf_t4a <- decrement_model_t4a_ %>% mutate_all(funs(na2zero)) # just for safety 
decrement_wf_t4b <- decrement_model_t4b_ %>% mutate_all(funs(na2zero)) # just for safety 
decrement_wf_t6  <- decrement_model_t6_  %>% mutate_all(funs(na2zero)) # just for safety 


# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 


## Where do the actives go

# Tier 4 basic
p_active2term_t4a    <- make_dmat("qxt", df = filter(decrement_wf_t4a, start_year == min(start_year)))
p_active2disbRet_t4a <- make_dmat("qxd", df = filter(decrement_wf_t4a, start_year == min(start_year)))
p_active2dead_t4a    <- make_dmat("qxm_actives", df = filter(decrement_wf_t4a, start_year == min(start_year)))
p_active2servRet_t4a <- make_dmat("qxr", df = filter(decrement_wf_t4a, start_year == min(start_year)))
p_active2la_t4a      <- make_dmat("qxr", df = filter(decrement_wf_t4a, start_year == min(start_year)))
p_active2deathBen_t4a<- make_dmat("qxm_actives", df = filter(decrement_wf_t4a, start_year == min(start_year)))

# Tier 4 55program
p_active2term_t4b    <- make_dmat("qxt", df = filter(decrement_wf_t4b, start_year == min(start_year)))
p_active2disbRet_t4b <- make_dmat("qxd", df = filter(decrement_wf_t4b, start_year == min(start_year)))
p_active2dead_t4b    <- make_dmat("qxm_actives", df = filter(decrement_wf_t4b, start_year == min(start_year)))
p_active2servRet_t4b <- make_dmat("qxr", df = filter(decrement_wf_t4b, start_year == min(start_year)))
p_active2la_t4b      <- make_dmat("qxr", df = filter(decrement_wf_t4b, start_year == min(start_year)))
p_active2deathBen_t4b<- make_dmat("qxm_actives", df = filter(decrement_wf_t4b, start_year == min(start_year))) 

# Tier 6
p_active2term_t6    <- make_dmat("qxt", df = filter(decrement_wf_t6, start_year == min(start_year)))
p_active2disbRet_t6 <- make_dmat("qxd", df = filter(decrement_wf_t6, start_year == min(start_year)))
p_active2dead_t6    <- make_dmat("qxm_actives", df = filter(decrement_wf_t6, start_year == min(start_year)))
p_active2servRet_t6 <- make_dmat("qxr", df = filter(decrement_wf_t6, start_year == min(start_year)))
p_active2la_t6      <- make_dmat("qxr", df = filter(decrement_wf_t6, start_year == min(start_year)))
p_active2deathBen_t6<- make_dmat("qxm_actives", df = filter(decrement_wf_t6, start_year == min(start_year))) 



## Where do the terminated go
# p_term2dead    <- make_dmat("qxm_terms", df = filter(decrement_wf, start_year == min(start_year))) 

# Tier 4 basic
p_term2dead_t4a <- expand.grid(ea  = range_ea, 
															age = range_age, 
															year = init_year:(init_year + nyear - 1), 
															year_term = (init_year):(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_term = age - (year - year_term)) %>% 
	left_join(decrement_wf_t4a %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_terms) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_terms = na2zero(qxm_terms)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_term, age, ea)


# Tier 4 55program
p_term2dead_t4b <- expand.grid(ea  = range_ea, 
															 age = range_age, 
															 year = init_year:(init_year + nyear - 1), 
															 year_term = (init_year):(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_term = age - (year - year_term)) %>% 
	left_join(decrement_wf_t4b %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_terms) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_terms = na2zero(qxm_terms)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_term, age, ea)


# Tier 6
p_term2dead_t6 <- expand.grid(ea  = range_ea, 
															 age = range_age, 
															 year = init_year:(init_year + nyear - 1), 
															 year_term = (init_year):(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_term = age - (year - year_term)) %>% 
	left_join(decrement_wf_t6 %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_terms) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_terms = na2zero(qxm_terms)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_term, age, ea)





## Where do the disabled go
#p_disbRet2dead    <- make_dmat("qxm_disbRet", df = filter(decrement_wf, start_year == min(start_year)))

# Tier 4 basic
p_disbRet2dead_t4a <- expand.grid(ea  = range_ea, 
												 age = range_age, 
												 year = init_year:(init_year + nyear - 1), 
												 year_disbRet = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_disbRet = age - (year - year_disbRet)) %>% 
	left_join(decrement_wf_t4a %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_disbRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_disbRet = na2zero(qxm_disbRet)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_disbRet, age, ea)


# Tier 4 55program
p_disbRet2dead_t4b <- expand.grid(ea  = range_ea, 
																	age = range_age, 
																	year = init_year:(init_year + nyear - 1), 
																	year_disbRet = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_disbRet = age - (year - year_disbRet)) %>% 
	left_join(decrement_wf_t4b %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_disbRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_disbRet = na2zero(qxm_disbRet)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_disbRet, age, ea)


# Tier 6
p_disbRet2dead_t6 <- expand.grid(ea  = range_ea, 
																	age = range_age, 
																	year = init_year:(init_year + nyear - 1), 
																	year_disbRet = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_disbRet = age - (year - year_disbRet)) %>% 
	left_join(decrement_wf_t6 %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_disbRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_disbRet = na2zero(qxm_disbRet)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_disbRet, age, ea)



## Where do the death beneficiaries go

p_deathBen2dead_t4a <- make_dmat("qxm_servRet", df = filter(decrement_wf_t4a, start_year == min(start_year))) # Simplified: weighted average of male and female mortality
p_deathBen2dead_t4b <- make_dmat("qxm_servRet", df = filter(decrement_wf_t4b, start_year == min(start_year))) # Simplified: weighted average of male and female mortality
p_deathBen2dead_t6  <- make_dmat("qxm_servRet", df = filter(decrement_wf_t6, start_year == min(start_year))) # Simplified: weighted average of male and female mortality




## Where do the retirees go 
# Before we find better approach, the age_servRet(retriement age) dependent mortality for retirees are given in a data frame containing all combos 
# of year, year_servRet (year of retirement), ea, and age that exist in wf_la. 

# N/A in TRS model V1 

# Tier 4 basic
p_la2dead_t4a <- expand.grid(ea  = range_ea, 
                         age = range_age, 
                         year = init_year:(init_year + nyear - 1), 
                         year_servRet = init_year:(init_year + nyear - 1)) %>%
  # filter(age >= ea) %>% 
  mutate(age_servRet = age - (year - year_servRet)) %>% 
  left_join(decrement_wf_t4a %>% 
  					  mutate(year = start_year + age - ea) %>% 
  					  select(year, ea, age, qxm_servRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_servRet = na2zero(qxm_servRet)) %>% 
  #left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
  # mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  arrange(year, year_servRet, age, ea)


# Tier 4 55program
p_la2dead_t4b <- expand.grid(ea  = range_ea, 
														 age = range_age, 
														 year = init_year:(init_year + nyear - 1), 
														 year_servRet = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_servRet = age - (year - year_servRet)) %>% 
	left_join(decrement_wf_t4b %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_servRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_servRet = na2zero(qxm_servRet)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_servRet, age, ea)


# Tier 6
p_la2dead_t6 <- expand.grid(ea  = range_ea, 
														 age = range_age, 
														 year = init_year:(init_year + nyear - 1), 
														 year_servRet = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_servRet = age - (year - year_servRet)) %>% 
	left_join(decrement_wf_t6 %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_servRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_servRet = na2zero(qxm_servRet)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_servRet, age, ea)




# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 


#*************************************************************************************************************
#                                     Creating a function to calculate new entrants ####
#*************************************************************************************************************


# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, dist, no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(max_retAge - 1)
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0; NE[ ,] <- 0
  
  if (no.entrants){ 
    return(NE) 
  } else {
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
  } 
}

# test the function 
# wf0 <- wf_active[, , 1]
# wf1 <- wf_active[, , 1]*(1 - p_active2term)
# sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
# sum(calc_entrants(wf0, wf1, 0), na.rm = T)


#*************************************************************************************************************
#                               Creating a function to calculate new entrants, FOR ALL TIERS ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants_allTiers <- function(wf0.t4a,  wf0.t4b,  wf0.t6, 
																	 wf1.t4a,  wf1.t4b,  wf1.t6, 
																	 dist.t4a, dist.t4b, dist.t6,
																	 newEnt_byTier_,
																	 delta, 
																	 no.entrants = FALSE){
	# This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
	# growth rate. 
	# inputs:
	# wf0: a matrix of workforce before decrement. Typically a slice from wf_active
	# wf1: a matrix of workforce after  decrement.  
	# delta: growth rate of workforce
	# newEnt_byTier: named vector, proportion of new entrants entering each tier. names must be "t4a", "t4b", "t6"
	# returns:
	# a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
	# and 0 in all other cells. 
	
	# working age
	working_age <- min(range_age):(max_retAge - 1) # FOR ALL TIERS
	# age distribution of new entrants
	# dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
	
	# compute the size of workforce before and after decrement
	size0.t4a <- sum(wf0.t4a[,as.character(working_age)], na.rm = TRUE)
	size1.t4a <- sum(wf1.t4a[,as.character(working_age)], na.rm = TRUE)
	
	size0.t4b <- sum(wf0.t4b[,as.character(working_age)], na.rm = TRUE)
	size1.t4b <- sum(wf1.t4b[,as.character(working_age)], na.rm = TRUE)
	
	size0.t6 <- sum(wf0.t6[,as.character(working_age)], na.rm = TRUE)
	size1.t6 <- sum(wf1.t6[,as.character(working_age)], na.rm = TRUE)
	
	
	
	# computing new entrants
	size_target <- (size0.t4a + size0.t4b + size0.t6) * (1 + delta)   # size of the workforce next year
	size_hire   <- size_target - (size1.t4a + size1.t4b + size1.t6)  # number of workers need to hire
	
	ne.t4a <- size_hire  * newEnt_byTier_["t4a"] * dist.t4a # vector, number of new entrants by age
	ne.t4b <- size_hire  * newEnt_byTier_["t4b"] * dist.t4b 
	ne.t6  <- size_hire  * newEnt_byTier_["t6"]  * dist.t6 
	
	
	# Create the new entrant matrix 
	NE.t4a <- wf0.t4a  ;  NE.t4a[ ,]  <- 0
	NE.t4b <- wf0.t4b  ;  NE.t4b[ ,]  <- 0
	NE.t6  <- wf0.t6   ;  NE.t6[ ,]   <- 0
	
	if (no.entrants){ 
		return(NE = list(NE.t4a = NE.t4a, NE.t4b = NE.t4b, NE.t6 = NE.t6)) 
		
	} else {
		NE.t4a[, rownames(NE.t4a)]  <- diag(ne.t4a) # place ne on the matrix of new entrants
		NE.t4b[,  rownames(NE.t4b)] <- diag(ne.t4b)
		NE.t6[,  rownames(NE.t6)]   <- diag(ne.t6)
		
		return(NE = list(NE.t4a = NE.t4a, NE.t4b = NE.t4b, NE.t6 = NE.t6)) 
	} 
}  



#*************************************************************************************************************
#                                     Simulating the evolution of population  ####
#*************************************************************************************************************

# Now the next slice of the array (array[, , i + 1]) is defined: 
 # wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
 # i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
  #j <- 1
	
	#*******************************************
	# Stage 1 Seperations by type in each tier *
	#*******************************************
	
	## Tier 4 basic
  	# compute the inflow to and outflow
  	active2term_t4a    <- wf_active_t4a[, , j] * p_active2term_t4a     # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  	active2servRet_t4a <- wf_active_t4a[, , j] * p_active2servRet_t4a  # This will be used to calculate the number of actives leaving the workforce
  	active2la_t4a      <- wf_active_t4a[, , j] * p_active2la_t4a
  	active2disbRet_t4a <- wf_active_t4a[, , j] * p_active2disbRet_t4a
  	active2dead_t4a    <- wf_active_t4a[, , j] * p_active2dead_t4a
  	active2deathBen_t4a<- wf_active_t4a[, , j] * p_active2deathBen_t4a 
  	
  	# Where do the terminated_vested go
  	# term2dead  <- wf_term[, , j, ] * as.vector(p_term2dead)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  	term2dead_t4a  <- wf_term_t4a[, , j, ] * (p_term2dead_t4a %>% filter(year == j + init_year - 1))[["qxm_terms"]] 
  	
  	# Where do the retired go
  	la2dead_t4a   <- wf_la_t4a[, , j, ] * (p_la2dead_t4a %>% filter(year == j + init_year - 1))[["qxm_servRet"]]  #[["qxm.post.W"]]  # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  	
  	# Where do the disabled la go
  	# disbRet2dead      <- wf_disbRet[, , j, ] * as.vector(p_disbRet2dead)
  	disbRet2dead_t4a  <- wf_disbRet_t4a[, , j, ] * (p_disbRet2dead_t4a %>% filter(year == j + init_year - 1))[["qxm_disbRet"]] 
  	
  	# Where do the death beneficiaries go
  	deathBen2dead_t4a  <- wf_deathBen_t4a[, , j, ] * as.vector(p_deathBen2dead_t4a)
  
  ## Tier 4 55program
  	# compute the inflow to and outflow
  	active2term_t4b    <- wf_active_t4b[, , j] * p_active2term_t4b     # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  	active2servRet_t4b <- wf_active_t4b[, , j] * p_active2servRet_t4b  # This will be used to calculate the number of actives leaving the workforce
  	active2la_t4b      <- wf_active_t4b[, , j] * p_active2la_t4b
  	active2disbRet_t4b <- wf_active_t4b[, , j] * p_active2disbRet_t4b
  	active2dead_t4b    <- wf_active_t4b[, , j] * p_active2dead_t4b
  	active2deathBen_t4b<- wf_active_t4b[, , j] * p_active2deathBen_t4b 
  	
  	# Where do the terminated_vested go
  	# term2dead  <- wf_term[, , j, ] * as.vector(p_term2dead)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  	term2dead_t4b  <- wf_term_t4b[, , j, ] * (p_term2dead_t4b %>% filter(year == j + init_year - 1))[["qxm_terms"]] 
  	
  	# Where do the retired go
  	la2dead_t4b   <- wf_la_t4b[, , j, ] * (p_la2dead_t4b %>% filter(year == j + init_year - 1))[["qxm_servRet"]]  #[["qxm.post.W"]]  # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  	
  	# Where do the disabled la go
  	# disbRet2dead      <- wf_disbRet[, , j, ] * as.vector(p_disbRet2dead)
  	disbRet2dead_t4b  <- wf_disbRet_t4b[, , j, ] * (p_disbRet2dead_t4b %>% filter(year == j + init_year - 1))[["qxm_disbRet"]] 
  	
  	# Where do the death beneficiaries go
  	deathBen2dead_t4b  <- wf_deathBen_t4b[, , j, ] * as.vector(p_deathBen2dead_t4b)
  
  ## Tier 6
  	# compute the inflow to and outflow
  	active2term_t6    <- wf_active_t6[, , j] * p_active2term_t6     # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  	active2servRet_t6 <- wf_active_t6[, , j] * p_active2servRet_t6  # This will be used to calculate the number of actives leaving the workforce
  	active2la_t6      <- wf_active_t6[, , j] * p_active2la_t6
  	active2disbRet_t6 <- wf_active_t6[, , j] * p_active2disbRet_t6
  	active2dead_t6    <- wf_active_t6[, , j] * p_active2dead_t6
  	active2deathBen_t6<- wf_active_t6[, , j] * p_active2deathBen_t6 
  	
  	# Where do the terminated_vested go
  	# term2dead  <- wf_term[, , j, ] * as.vector(p_term2dead)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  	term2dead_t6  <- wf_term_t6[, , j, ] * (p_term2dead_t6 %>% filter(year == j + init_year - 1))[["qxm_terms"]] 
  	
  	# Where do the retired go
  	la2dead_t6   <- wf_la_t6[, , j, ] * (p_la2dead_t6 %>% filter(year == j + init_year - 1))[["qxm_servRet"]]  #[["qxm.post.W"]]  # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  	
  	# Where do the disabled la go
  	# disbRet2dead      <- wf_disbRet[, , j, ] * as.vector(p_disbRet2dead)
  	disbRet2dead_t6  <- wf_disbRet_t6[, , j, ] * (p_disbRet2dead_t6 %>% filter(year == j + init_year - 1))[["qxm_disbRet"]] 
  	
  	# Where do the death beneficiaries go
  	deathBen2dead_t6  <- wf_deathBen_t6[, , j, ] * as.vector(p_deathBen2dead_t6)
  
  #***********************************
  # Stage 2 Seperations in each tier *
  #***********************************
  
  # Total inflow and outflow for each status
  out_active_t4a   <- active2term_t4a + active2disbRet_t4a + active2servRet_t4a + active2dead_t4a 
  out_active_t4b   <- active2term_t4b + active2disbRet_t4b + active2servRet_t4b + active2dead_t4b 
  out_active_t6    <- active2term_t6  + active2disbRet_t6  + active2servRet_t6  + active2dead_t6 
  
  # new_entrants <- calc_entrants(wf_active[, , j], wf_active[, , j] - out_active, wf_growth, dist = entrants_dist_, no.entrants = no_entrants) # new entrants
  
  new_entrants_allTiers <- 
  	calc_entrants_allTiers(wf_active_t4a[, , j], 
  												 wf_active_t4b[, , j], 
  												 wf_active_t6[, , j], 
  												 
  												 
  												 wf_active_t4a[, , j]  - out_active_t4a, 
  												 wf_active_t4b[, , j]  - out_active_t4b,
  												 wf_active_t6[, , j]   - out_active_t6,
  												 
  												 
  												 entrants_dist_,
  												 entrants_dist_,
  												 entrants_dist_,
  												 
  												 newEnt_byTier,
  												 
  												 wf_growth,
  												 
  												 no.entrants = no_entrants) # new entrants
  
 
 	## Tier 4 basic  
  out_term_t4a <- term2dead_t4a    # This is a 3D array 
  in_term_t4a  <- active2term_t4a  # This is a matrix
  
  out_disbRet_t4a <- disbRet2dead_t4a
  in_disbRet_t4a  <- active2disbRet_t4a
  
  out_la_t4a <- la2dead_t4a        # This is a 3D array (ea x age x year.retire)
  in_la_t4a  <- active2la_t4a      # This is a matrix
  
  out_deathBen_t4a <- deathBen2dead_t4a        # This is a 3D array (ea x age x year.retire)
  in_deathBen_t4a  <- active2deathBen_t4a     # This is a matrix
  
  in_dead_t4a <- active2dead_t4a +                                             
                 apply(term2dead_t4a,    c(1,2), sum) +   # 
                 apply(la2dead_t4a,      c(1,2), sum) +   # get a matirix of ea x age by summing over year.term/year.retiree
                 apply(disbRet2dead_t4a, c(1,2), sum) 
  
  
  ## Tier 4 55program  
  out_term_t4b <- term2dead_t4b    # This is a 3D array 
  in_term_t4b  <- active2term_t4b  # This is a matrix
  
  out_disbRet_t4b <- disbRet2dead_t4b
  in_disbRet_t4b  <- active2disbRet_t4b
  
  out_la_t4b <- la2dead_t4b        # This is a 3D array (ea x age x year.retire)
  in_la_t4b  <- active2la_t4b      # This is a matrix
  
  out_deathBen_t4b <- deathBen2dead_t4b        # This is a 3D array (ea x age x year.retire)
  in_deathBen_t4b  <- active2deathBen_t4b     # This is a matrix
  
  in_dead_t4b <- active2dead_t4b +                                             
  	apply(term2dead_t4b,    c(1,2), sum) +   # 
  	apply(la2dead_t4b,      c(1,2), sum) +   # get a matirix of ea x age by summing over year.term/year.retiree
  	apply(disbRet2dead_t4b, c(1,2), sum) 
  
  
  ## Tier 6   
  out_term_t6 <- term2dead_t6    # This is a 3D array 
  in_term_t6  <- active2term_t6  # This is a matrix
  
  out_disbRet_t6 <- disbRet2dead_t6
  in_disbRet_t6  <- active2disbRet_t6
  
  out_la_t6 <- la2dead_t6        # This is a 3D array (ea x age x year.retire)
  in_la_t6  <- active2la_t6      # This is a matrix
  
  out_deathBen_t6 <- deathBen2dead_t6        # This is a 3D array (ea x age x year.retire)
  in_deathBen_t6  <- active2deathBen_t6     # This is a matrix
  
  in_dead_t6 <- active2dead_t6 +                                             
  	apply(term2dead_t6,    c(1,2), sum) +   # 
  	apply(la2dead_t6,      c(1,2), sum) +   # get a matirix of ea x age by summing over year.term/year.retiree
  	apply(disbRet2dead_t6, c(1,2), sum) 
  
  
  
  #*********************************************
  # Stage 3  Calculate workforce for next year. 
  #*********************************************  
 

  ## Tier 4 basic
  wf_active_t4a[, , j + 1]  <- (wf_active_t4a[, , j] - out_active_t4a) %*% A + new_entrants_allTiers$NE.t4a
  
  wf_term_t4a[, , j + 1, ]      <- apply((wf_term_t4a[, , j, ] - out_term_t4a), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term_t4a[, , j + 1, j + 1] <- in_term_t4a %*% A     # Note that termination year j = 1 correponds to init_year - 1?
  
  wf_la_t4a[, ,j + 1, ]       <- apply((wf_la_t4a[, , j, ] - out_la_t4a), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la_t4a[, , j + 1, j + 1] <- in_la_t4a %*% A
  
  wf_disbRet_t4a[, , j + 1, ]      <- apply((wf_disbRet_t4a[, , j, ] - out_disbRet_t4a), 3, function(x) x %*% A) %>% array(wf_dim.disb[-3])
  wf_disbRet_t4a[, , j + 1, j + 1] <- in_disbRet_t4a %*% A
  
  wf_dead_t4a[, ,   j + 1]    <- (wf_dead_t4a[, , j] + in_dead_t4a) %*% A
  
  wf_deathBen_t4a[, , j + 1, ]      <- apply((wf_deathBen_t4a[, , j, ] - out_deathBen_t4a), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
  wf_deathBen_t4a[, , j + 1, j + 1] <- in_deathBen_t4a %*% A
 
  
  newDeath.act_t4a[j]  <- sum(active2dead_t4a)
  newDeath.ret_t4a[j]  <- sum(la2dead_t4a)
  newDisb.act_t4a[j]   <- sum(active2disbRet_t4a)
  
  
  ## Tier 4 55program
  wf_active_t4b[, , j + 1]  <- (wf_active_t4b[, , j] - out_active_t4b) %*% A + new_entrants_allTiers$NE.t4b
  
  wf_term_t4b[, , j + 1, ]      <- apply((wf_term_t4b[, , j, ] - out_term_t4b), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term_t4b[, , j + 1, j + 1] <- in_term_t4b %*% A     # Note that termination year j = 1 correponds to init_year - 1?
  
  wf_la_t4b[, ,j + 1, ]       <- apply((wf_la_t4b[, , j, ] - out_la_t4b), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la_t4b[, , j + 1, j + 1] <- in_la_t4b %*% A
  
  wf_disbRet_t4b[, , j + 1, ]      <- apply((wf_disbRet_t4b[, , j, ] - out_disbRet_t4b), 3, function(x) x %*% A) %>% array(wf_dim.disb[-3])
  wf_disbRet_t4b[, , j + 1, j + 1] <- in_disbRet_t4b %*% A
  
  wf_dead_t4b[, ,   j + 1]    <- (wf_dead_t4b[, , j] + in_dead_t4b) %*% A
  
  wf_deathBen_t4b[, , j + 1, ]      <- apply((wf_deathBen_t4b[, , j, ] - out_deathBen_t4b), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
  wf_deathBen_t4b[, , j + 1, j + 1] <- in_deathBen_t4b %*% A
  
  
  newDeath.act_t4b[j]  <- sum(active2dead_t4b)
  newDeath.ret_t4b[j]  <- sum(la2dead_t4b)
  newDisb.act_t4b[j]   <- sum(active2disbRet_t4b)
  
  
  ## Tier 6 
  wf_active_t6[, , j + 1]  <- (wf_active_t6[, , j] - out_active_t6) %*% A + new_entrants_allTiers$NE.t6
  
  wf_term_t6[, , j + 1, ]      <- apply((wf_term_t6[, , j, ] - out_term_t6), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term_t6[, , j + 1, j + 1] <- in_term_t6 %*% A     # Note that termination year j = 1 correponds to init_year - 1?
  
  wf_la_t6[, ,j + 1, ]       <- apply((wf_la_t6[, , j, ] - out_la_t6), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la_t6[, , j + 1, j + 1] <- in_la_t6 %*% A
  
  wf_disbRet_t6[, , j + 1, ]      <- apply((wf_disbRet_t6[, , j, ] - out_disbRet_t6), 3, function(x) x %*% A) %>% array(wf_dim.disb[-3])
  wf_disbRet_t6[, , j + 1, j + 1] <- in_disbRet_t6 %*% A
  
  wf_dead_t6[, ,   j + 1]    <- (wf_dead_t6[, , j] + in_dead_t6) %*% A
  
  wf_deathBen_t6[, , j + 1, ]      <- apply((wf_deathBen_t6[, , j, ] - out_deathBen_t6), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
  wf_deathBen_t6[, , j + 1, j + 1] <- in_deathBen_t6 %*% A
  
  
  newDeath.act_t6[j]  <- sum(active2dead_t6)
  newDeath.ret_t6[j]  <- sum(la2dead_t6)
  newDisb.act_t6[j]   <- sum(active2disbRet_t6)
  
  
  # active2la
  # wf_active[,,j]
  
}





#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

get_df.wf_active <- function(df){ 
	adply(df, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, number.a, -ea, -year) %>% 
  mutate(year = f2n(year), age = as.numeric(age)) %>% 
  filter(age >= ea)
}

wf_active_t4a <- get_df.wf_active(wf_active_t4a) 
wf_active_t4b <- get_df.wf_active(wf_active_t4b) 
wf_active_t6  <- get_df.wf_active(wf_active_t6) 



get_df.wf_la <- function(df){ 
	data.frame(expand.grid(ea   = range_ea, 
                         age  = range_age, 
                         year = init_year:(init_year + nyear - 1), 
                         year_servRet = init_year:(init_year + nyear - 1)),
                         number.la = as.vector(df)) %>% 
         filter(age >= ea)
}

wf_la_t4a <- get_df.wf_la(wf_la_t4a)
wf_la_t4b <- get_df.wf_la(wf_la_t4b)
wf_la_t6  <- get_df.wf_la(wf_la_t6)




get_df.wf_term <- function(df){
	data.frame(expand.grid(ea   = range_ea, 
                         age  = range_age, 
                         year = init_year:(init_year + nyear - 1), 
                         year_term = (init_year):(init_year + nyear - 1)),
                         number.v = as.vector(df)) %>% 
         filter(age >= ea)
}

wf_term_t4a <- get_df.wf_term(wf_term_t4a)
wf_term_t4b <- get_df.wf_term(wf_term_t4b)
wf_term_t6  <- get_df.wf_term(wf_term_t6)



get_df.wf_disbRet <- function(df){ 
	data.frame(expand.grid(ea   = range_ea, 
                         age  = range_age, 
                         year = init_year:(init_year + nyear - 1), 
                         year_disbRet = (init_year):(init_year + nyear - 1)),
                         number.disbRet = as.vector(df)) %>% 
               filter(age >= ea)
}
	
wf_disbRet_t4a <- get_df.wf_disbRet(wf_disbRet_t4a)
wf_disbRet_t4b <- get_df.wf_disbRet(wf_disbRet_t4b)
wf_disbRet_t6  <- get_df.wf_disbRet(wf_disbRet_t6)




get_df.wf_deathBen <- function(df){
	data.frame(expand.grid(ea   = range_ea, 
												 age  = range_age, 
												 year = init_year:(init_year + nyear - 1), 
												 year_death = (init_year):(init_year + nyear - 1)),
                         number.deathBen = as.vector(df)) %>%
                         filter(age >= ea)
}

wf_deathBen_t4a  <- get_df.wf_deathBen(wf_deathBen_t4a)
wf_deathBen_t4b  <- get_df.wf_deathBen(wf_deathBen_t4b)
wf_deathBen_t6   <- get_df.wf_deathBen(wf_deathBen_t6)

#wf_deathBen %>% filter(number.deathBen != 0) 


# Final outputs
pop <- list(
	pop_t4a = list(active  = wf_active_t4a, 
                 term    = wf_term_t4a, 
                 disbRet = wf_disbRet_t4a, 
                 la      = wf_la_t4a, 
                 dead    = wf_dead_t4a,
                 deathBen= wf_deathBen_t4a),
	
	pop_t4b = list(active  = wf_active_t4b, 
								 term    = wf_term_t4b, 
								 disbRet = wf_disbRet_t4b, 
								 la      = wf_la_t4b, 
								 dead    = wf_dead_t4b,
								 deathBen= wf_deathBen_t4b),
	
	pop_t6 = list(active  = wf_active_t6, 
								 term    = wf_term_t6, 
								 disbRet = wf_disbRet_t6, 
								 la      = wf_la_t6, 
								 dead    = wf_dead_t6,
								 deathBen= wf_deathBen_t6)
			 
)

return(pop)

}


# pop <- get_Population()






# pop$term %>% filter(year == 2016) %>% select(number.v) %>% sum



# # Spot check the results
# wf_active %>% group_by(year) %>% summarise(n = sum(number.a)) %>% mutate(x = n == 1000) %>% data.frame # OK
# wf_active %>% filter(year == 2025) %>% spread(age, number.a)
# 
# 
# wf_la %>% group_by(year) %>% summarise(n = sum(number.la)) %>% data.frame  
# 
# wf_la %>% filter(year.r == 2016, year == 2018, age==65) %>% mutate(number.la_next = number.la * 0.9945992) %>% 
#   left_join(wf_la %>% filter(year.r == 2016, year == 2019, age==66) %>% select(year.r, ea, number.la_true = number.la)) %>% 
#   mutate(diff = number.la_true - number.la_next) # looks ok.
# 
# mortality.post.ucrp %>% filter(age.r == 63)
# 
# 
# 
# 
# # check retirement
# wf_active %>% filter(year == 2020, ea == 30) %>% select(-year) %>% 
# left_join(wf_la     %>% filter(year == 2021, year.r == 2021, ea == 30)) %>% 
# left_join(wf_LSC.ca %>% filter(year == 2021, ea == 30) %>% select(year, ea, age, new_LSC, new_ca)) %>% 
# left_join(decrement_wf %>% filter(ea == 30) %>% select(ea, age, qxr, qxr.la, qxr.ca, qxr.LSC)) %>% 
# filter(age >= 49 & age <=75) %>% 
# mutate(diff.la = lag(number.a *qxr.la) - number.la,
#        diff.ca = lag(number.a *qxr.ca) - new_ca,
#        diff.LSC= lag(number.a *qxr.LSC) - new_LSC,
#        diff.r  = lag(number.a *qxr) - (new_ca + new_LSC + number.la))
#   # looks ok.
#





# wf_active %>% group_by(year) %>% summarise(n = sum(number.a))
# 
# wf_active %>% head
# 



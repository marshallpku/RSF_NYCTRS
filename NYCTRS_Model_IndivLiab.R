# This script calculates individdual liabilities and normal costs for NYCTRS 
# version 1
#  - basic Tier 4 provisions
#  - simple COLA
#  - service retirement benefit and deferred retirement benefit only

# Road map

# 1.  Preparation

# 2.1 AL and NC of life annuity and contingent annuity for actives
# 2.2 AL and benefit for retirees with life annuity

# 3.1 AL and NC of deferred benefits for actives
# 3.2 AL and benefits for vested terminated members

# 4.1 AL and NC of benefit for death before retirement
# 4.2 AL and benefits for vested terminated members

# 5.1 AL and NC of disability benefit
# 5.2 AL and benefits for disability benefit

# 6. Selecting variables for the chosen actuarial method





get_indivLab <- function(tier_select_,
                         decrement_model_ = decrement_model,
                         salary_          = salary,
                         benefit_servRet_ = benefit_servRet,
                         # benefit_disbRet_ = benefit_disbRet,
                         # bfactor_         = bfactor,
                         # mortality.post.model_ = mortality.post.model,
                         #liab.ca_ = liab.ca,
                         #liab.disb.ca_ = liab.disb.ca,
                         init_terms_ = initPop$terms,
                         paramlist_ = paramlist,
                         Global_paramlist_ = Global_paramlist){

# Inputs
  # decrement_model_ = decrement_model
  # salary_          = salary
  # benefit_servRet_ = benefit_servRet
  # 
  # init_terms_      = initPop$terms # get_tierData(init_terms_all, Tier_select)
  # tier_select_     = paramlist$tier_select
  # 
  # #benefit_disbRet_ = benefit_disbRet
  # #mortality.post.model_ = mortality.post.model
  # #liab.ca_         = liab.ca
  # #liab.disb.ca_ = liab.disb.ca
  # 
  # paramlist_       =  paramlist
  # Global_paramlist_ =  Global_paramlist
  #  
  # Tier_select_ = "tE"
  # decrement.model_ = decrement.model.tE
  # salary_          = salary.tE
  # benefit_         = benefit.tE
  # benefit.disb_    = benefit.disb.tE
  # #bfactor_         = bfactor.tE
  # mortality.post.model_ = mortality.post.model.tE
  # liab.ca_ = liab.ca.tE
  # liab.disb.ca_ = liab.disb.ca.tE
  # init_terms_all_  = init_terms_all
  # paramlist_ = paramlist
  # Global_paramlist_ = Global_paramlist
  
  assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
  assign_parmsList(paramlist_,        envir = environment())
  

# Choosing tier specific parameters and data
# fasyears <- tier.param[Tier_select_, "fasyears"]
# r.vben   <- tier.param[Tier_select_, "r.vben"]
# r.yos    <- tier.param[Tier_select_, "r.yos"]
# r.age    <- tier.param[Tier_select_, "r.age"]
# v.yos    <- tier.param[Tier_select_, "v.yos"]
# EEC.rate <- tier.param[Tier_select_, "EEC.rate"]


# init_terminated_ <-  get_tierData(init_terms_all_, Tier_select_)
# init_terminated_ %<>% mutate(benefit = 0.75 * 0.42 * termCon) 

  
#*************************************************************************************************************
#                               1. Preparation                        #####                  
#*************************************************************************************************************
cat("Preparation\n")
# Specify the earliest year needed
# Ealiest year required for actives：the year a (max_retAge - 1) year old active in year 1 enter the workforce at age min_ea 

min_year_actives <- init_year - ( (max_retAge - 1) -  min_ea)
min_year <- min_year_actives
    
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
## Track down to the year that is the smaller one of the two below:

 # min_year <- min(init_year - (max_age - (r_max - 1)), 
 #                 init_year - (r_max - 1 - min_ea)) 
 #                # min(init.year - (benefit_$age - (r.min - 1))))
 

liab_active <- 
  expand.grid(start_year = min_year:(init_year + nyear - 1) , 
              ea         = range_ea, 
              age        = range_age) %>%
  filter(start_year + max_age - ea >= init_year, # drop redundant combinations of start_year and ea. (delet those who never reach year 1.) 
                                                 # max_age can replaced by max_regAge - 1 if only consider actives
         age >= ea) %>%   
  mutate(year = start_year + age - ea) %>%            
  arrange(start_year, ea, age) %>% 
  left_join(salary_) %>%
  left_join(decrement_model_) %>% 
  # left_join(mortality.post.model_ %>% filter(age == age.r) %>% ungroup %>% select(year, age, ax.r.W)) %>%
  # left_join(liab.ca_      %>% filter(age == age.r)    %>% ungroup %>% select(year, age, liab.ca.sum.1)) %>% 
  # left_join(liab.disb.ca_ %>% filter(age == age.disb) %>% ungroup %>% select(year, age, liab.disb.ca.sum.1 = liab.ca.sum.1)) %>% 
	
	mutate(tier_select = tier_select_,
				yos          = age - ea) %>% 
  group_by(start_year, ea) %>%
  
  
  # Calculate salary and benefits
  mutate(
    
    # Calculate Final Average Salary
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
    n  = pmin(yos, fasyears),                                      # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    
    # COLA
    COLA.scale = (1 + cola)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    
    # Accrued service retirement benefits for tier III/IV
    Bx = case_when(                                  # na2zero(bfactor * yos * fas), # accrued benefits, note that only Bx for ages above min retirement age are necessary under EAN.
    	yos < 20       ~ na2zero(5/300 * yos * fas),
    	yos %in% 20:29 ~ na2zero(0.02  * yos * fas),
    	yos > 30	     ~ na2zero( (0.6 + (yos - 30) * 0.015) * fas),
    	TRUE           ~ 0),                        
    bx = lead(Bx) - Bx,                              # benefit accrual at age x

    # ax.XXX: actuarial present value of future benefit, for $1's benefit in the initial year. 
      # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    
    ax.servRet  = get_tla(pxm_servRet, i, COLA.scale),      # Service retirement benefit (will be replaced when contingent retirement beneift is added) ax.r
    ax.terms    = get_tla(pxm_terms,   i, COLA.scale),       # deferred retirement benefit (actually we only need the value at age_vben)
    # ax.disbRet  = get_tla(pxm_disbRet, i, COLA.scale),     # disability retirement benefit
    # ax.r.W.ret is already in mortality.post.model_
    # ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale),
    
    
    # Temporary annuity values from age x to retirment age (fixed end)
     
     # for service retirement
    axR = c(get_tla(pxT[age < max_retAge], i), rep(0, max_age - max_retAge + 1)),                          # aT..{x:max_retAge-x-|} discount value of max_retAge at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < max_retAge], i,  sx[age < max_retAge]), rep(0, max_age - max_retAge + 1)),   # ^s_aT..{x:max_retAge-x-|}
     
     #for deferred retirement 
    axr = ifelse(ea >= age_vben, 0, c(get_tla(pxT[age < age_vben], i), rep(0, max_age - unique(age_vben) + 1))),                      # Similar to axR, but based  on age_superFirst. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)       
    axrs= ifelse(ea >= age_vben, 0, c(get_tla(pxT[age < age_vben], i,  sx[age < age_vben]), rep(0, max_age - unique(age_vben) + 1))),  # Similar to axRs, but based on age_superFirst. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)
    
    
    # temporary annuity values from a fixed entry age y to x (fixed start) (PV of future salary at entry age w/0 and w/ salary growth)
    ayx = c(get_tla2(pxT[age <= max_retAge], i), rep(0, max_age - max_retAge)),                          # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= max_retAge], i,  sx[age <= max_retAge]), rep(0, max_age - max_retAge))   # need to make up the length of the vector up to age max.age
    
    
    # axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    # axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
   
  )


# liab.active %>% select(ea, age, ax.disb.la, ax.vben)



#*************************************************************************************************************
#                        2.1  ALs and NCs of life annuity and contingent annuity for actives             #####                  
#*************************************************************************************************************
cat("Service Retirement - actives\n")

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab_active %<>%   
  mutate(
  	
  	# Benefit reduction factor for Tier IV: 6% for 60-61, 3% for 55-59
  	benReduction = case_when(
  		age %in% 60:61 ~ 1 - (62 - age) * 0.06,
  		age %in% 55:59 ~ 0.88 - (60 - age) *  0.03,
  		TRUE ~ 1),
    
  	# Retirement eligibility and % benefit can be claimed at retirement 
    gx.laca = case_when(
    	as.logical(elig_full) ~ 1, 
    	as.logical(elig_early) ~ benReduction,
    	TRUE ~ 0), 
    # gx.laca = 0,
    
  	# Calculate term costs 
  	Bx.laca  = gx.laca * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
  	TCx.la   = lead(Bx.laca) * qxr.la * lead(ax.servRet) * v,
  	# TCx.la   = lead(Bx.laca) * qxr.la * lead(ax.r.W) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
  	# TCx.ca   = lead(Bx.laca) * qxr.ca * lead(liab.ca.sum.1) * v,  # term cost of contingent annuity at the internal retirement age x (start to claim benefit at age x + 1)
  	TCx.laca =  TCx.la,   # TCx.la + TCx.ca,
  
    # Present value of future benefits and present value of future salary
    PVFBx.laca  = c(get_PVFB(pxT[age <= max_retAge], v, TCx.laca[age <= max_retAge]), rep(0, max_age - max_retAge)),
    PVFSx       = c(get_PVFB(pxT[age <= max_retAge], v, sx[age <= max_retAge]),       rep(0, max_age - max_retAge)),
  
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),

  # NC and AL of EAN.CD
  NCx.EAN.CD.laca = ifelse(age < max_retAge, PVFBx.laca[age == min(age)]/ayx[age == max_retAge], 0),
  ALx.EAN.CD.laca = PVFBx.laca - NCx.EAN.CD.laca * axR,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.laca   = ifelse(age < max_retAge, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == max_retAge]), 0),
  PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
  ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 


# x <- liab.active %>%
#   select(start.year, year, age, ea, NCx.EAN.CP.laca, PVFNC.EAN.CP.laca, ALx.EAN.CP.laca, PVFBx.laca, TCx.ca, TCx.la,TCx.laca,Bx.laca, liab.ca.sum.1, sx)
# x %>% filter(start.year == 2017, ea == 30)

# x <- liab.active %>%
#   select(start.year, year, age, ea, NCx.EAN.CP.laca, PVFNC.EAN.CP.laca, ALx.EAN.CP.laca, PVFBx.laca, TCx.la,TCx.laca,Bx.laca, qxr.la, ax.r.W, ayx, axRs, pxT)
# x %>% filter(start.year == 2017, ea == 30) 
# 



#*************************************************************************************************************
#                       2.2   ALs and benefits for retirees with life annuity                        #####                  
#*************************************************************************************************************
cat("Service Retirement - retirees\n")

# Calculate AL and benefit payment for retirees having retired at different ages.

liab_la <- rbind(
  
	# grids for initial retirees in year 1
    # To simplified the calculation, it is assmed that all initial retirees entered the workforce at age r.min - 1 and 
    # retiree right in year 1. This assumption will cause the retirement age and yos of some of the retirees not compatible with the eligiblility rules,
    # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial retirees/beneficiaries.
  
  expand.grid(
    age_servRet= filter(benefit_servRet_, benefit_servRet != 0)$age, # This ensures that year of retirement is year 1.
    age        = range_age[range_age >= min(filter(benefit_servRet_, benefit_servRet != 0)$age)]) %>%
    mutate(ea         = min_age,  # min(benefit_$age) - 1,
           start_year = init_year - (age_servRet - ea)) %>% 
    filter(# age >= ea, 
           age >= age_servRet),
 
  
  # grids for who retire after year 1.
  expand.grid(ea           = range_ea[range_ea < max_retAge],
              age_servRet  = min_retAge:max_retAge,
              start_year = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1), # LHS: entry year for those who have entry age of min_ea and retire in year 2 at max_retAge. 
              age        = range_age[range_age >=min_retAge]) %>%
    filter(age         >= ea,
           age_servRet >= ea,
           age         >= age_servRet,
           start_year + (age_servRet - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
           start_year + age - ea           >= init_year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start_year,ea,age_servRet,age")

liab_la <- liab_la[!duplicated(liab_la %>% select(start_year, ea, age, age_servRet ))]  # should have no duplication, just for safety


# x <- liab.la %>% mutate(year   = start.year + age - ea, year.r = start.year + age.r - ea) %>% filter(year == 2015, year.r == 2015)
# x
# benefit_%>% mutate(year   = start.year + age - ea, year.r = start.year + age.r - ea)


liab_la <- merge(liab_la,
                 select(liab_active, start_year, ea, age, Bx.laca, gx.laca, sx, ax.servRet) %>% data.table(key = "ea,age,start_year"),
                 all.x = TRUE, 
                 by = c("ea", "age","start_year")) %>%
           arrange(start_year, ea, age_servRet) %>% 
           mutate(year   = start_year + age - ea) %>% 
           as.data.frame %>% 
           # left_join(select(mortality.post.model_, year, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
           left_join(benefit_servRet_)

# liab.la %>% filter(start.year == 1990, ea == 35, age.r == 62)


liab_la %<>% as.data.frame  %>% 
  group_by(start_year, ea, age_servRet) %>%
  mutate(
    COLA.scale = (1 + cola)^(age - min(age)), # COLA.scale from liab.active may not cover all starting years.    
    year   = start_year + age - ea,
    year_servRet = start_year + age_servRet - ea,   # year of retirement
    
    # Calculate benefits (need to change for TRS COLA, calculate benefit payments)
    Bx.laca  = ifelse(is.na(Bx.laca), 0, Bx.laca),  # just for safety. Bx.laca is only needed for new retirees (retired in year 2 or later)
    B.la     = ifelse(year_servRet <= init_year,
                      benefit_servRet[year == init_year] * COLA.scale / COLA.scale[year == init_year],  # Benefits for initial retirees
                      Bx.laca[age == age_servRet] * COLA.scale / COLA.scale[age == age_servRet]),                   # Benefits for retirees after year 1
    
    # Liability of retirees (need to change for TRS COLA; based on payment cashflow, not using ax.xxx)
    ALx.la = B.la * ax.servRet     # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
    #ALx.la = B.la * ax.r.W.ret                                                                    

    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init_year, len = nyear)) %>%
  select(year, ea, age, year_servRet, age_servRet, start_year, B.la, ALx.la) %>%  
  			 # sx, ax.r.W.ret, benefit, Bx.laca, COLA.scale) %>% 
  arrange(age_servRet, start_year, ea, age)


# Spot check
# liab.la %>% filter(start.year == 2016, ea == 35 , age.r == 61) %>% as.data.frame() %>% ungroup() %>%  arrange(ea, year.r)



#*************************************************************************************************************
#                         3.1 AL and NC of deferred benefits for actives                        #####
#*************************************************************************************************************
cat("Deferred Retirement - actives\n")
# Calculate normal costs and liabilities of deferred retirement benefits
  # Vested terms begin to receive deferred retirement benefit at age_vben, after which they are considered as retirees in the AV.(But still terms in the model)

# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age age_vben - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: There will be a problem if actives entering after min_retAge can get vested, when PVFB is only amortized up to age min_retAge


liab_active %<>%
  mutate(gx.v = yos >= 5,  #  ifelse(elig_vest == 1, 1, 0),  # actives become vested after reaching v.yos years of yos
         
         Bx.v = 0, #ifelse(ea < age_vben, 
                  #      gx.v * bfactor * yos * fas * 0.85, 
         				  # 		 0), # initial annuity amount when the vested term retires at age r.vben, when a employee is vested at a certain age.
         
         #TCx.v  = ifelse(ea < r.vben, Bx.v * qxt * lead(px_r.vben_m) * v^(r.vben - age) * ax.r.W[age == r.vben], 0),             # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         TCx.v   = ifelse(ea < age_vben, qxt * lead(px_r.vben_m) * (Bx.v * ax.terms[age == age_vben]) *  v^(age_vben - age) , 0), # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         
         PVFBx.v = ifelse(ea < age_vben, c(get_PVFB(pxT[age < age_vben], v, TCx.v[age < age_vben]), rep(0, max_age - age_vben + 1)), 0),  # To be compatible with the cases where workers enter after age age_vben, max_retAge is used instead of min_retAge, which is used in textbook formula(winklevoss p115).
         
         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         
         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < age_vben, PVFBx.v[age == min(age)]/ayx[age == age_vben], 0), # for testing spreading NC.v up to r.vben
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,
         
         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < age_vben, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == age_vben]) * sx, 0),  # for testing spreading NC.v up to r.vben
         PVFNC.EAN.CP.v = NCx.EAN.CP.v * axrs,
         ALx.EAN.CP.v = PVFBx.v - PVFNC.EAN.CP.v
  ) 
  
# x <- liab.active %>% filter(start.year == 1, ea == 20)



#*************************************************************************************************************
#                       3.2 AL for vested terminatede members                        #####
#*************************************************************************************************************
cat("Deferred Retirement - retirees\n")
## Calculate AL and benefit payment for initial vested terms.

# init_terminated_ %<>%  
#   mutate(year = init.year,
#          age.term = age - 1)         # assume all terms are terminated in init.year - 1.
#          #yos = age - ea,
#          #start.year = year - (age - ea))
# 
# # init_terminated_
# 
# 
# # liab.term.init <- expand.grid(ea         = unique(init_terminated_$ea),
# #                               age.term   = unique(init_terminated_$age.term),
# #                               start.year = unique(init_terminated_$start.year),
# #                               age = range_age) %>%
# 
# liab.term.init <- expand.grid(age.term = unique(init_terminated_$age.term),
#                               age = range_age) %>% 
#   mutate(ea   = min(init_terminated_$ea),
#          year = init.year + (age - age.term - 1),
#          start.year = year - (age - ea)
#          ) %>% 
#   filter(start.year + age - ea >= 1,
#          age >= ea,
#          age.term >= ea) %>%
#   left_join(init_terminated_ %>% select(age.term, age, benefit.term = benefit)) %>%
#   left_join(select(liab.active, start.year, ea, age, COLA.scale, pxRm, px_r.vben_m, ax.vben)) %>%
#   left_join(decrement.model_ %>% select(start.year, ea, age, pxm.term)) %>% 
#   group_by(start.year, ea, age.term) %>%
# 
#   mutate(
#     year = start.year + age - ea,
#     age.ben =  ifelse(age[year == init.year] > r.vben, age[year == init.year], r.vben), # Age at which term starts to receive benefit. 
#     year.term = year[age == age.term],
# 
#     COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
#     ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#     
#     
#     Bx.v  = benefit.term,
#     
#     B.v   = ifelse(age.ben > r.vben, 0,    ifelse(age >= r.vben, Bx.v[age == unique(age.term) + 1]  * COLA.scale/COLA.scale[age == r.vben],  0)), # Benefit payment after r.vben, for age.ben == r.vben
#     B.v   = ifelse(age.ben == r.vben, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term) + 1] * COLA.scale/COLA.scale[age == age.ben], 0)), # for age.ben > r.vben
#     ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term) + 1] * ax.vben[age == r.vben] * px_r.vben_m * v^(r.vben - age), # liab before receiving benefits
#                    B.v * ax.vben)
#     ) %>%                                                                                     # liab after receiving benefits      
#   ungroup %>%
#   select(ea, age, start.year, year, year.term, B.v, ALx.v, ax.vben, pxm.term) %>%
#   filter(year %in% seq(init.year, len = nyear),
#          year.term == init.year - 1)
# 
# 
# 
# ##  Calculate AL and benefit payment for vested terms terminating at different ages.
# # Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
# liab.term <- expand.grid(# start.year   = (init.year - (r.vben - 1 - min.age)):(init.year + nyear - 1), # 2015
#                          start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
#                          ea = range_ea[range_ea < r.max], 
#                          age = range_age, 
#                          age.term = range_age[range_age < r.max]) %>% 
#   filter(start.year + max.age - ea >= init.year, 
#          age >= ea, age.term >= ea,
#          age >= age.term) %>% # drop redundant combinations of start.year and ea.
#   data.table(key = "ea,age,start.year,age.term") 
# 
# 
# liab.term <- merge(liab.term,
#                    select(liab.active, start.year, year, ea, age, Bx.v, COLA.scale, pxRm, px_r.vben_m, ax.vben, pxm.term) %>% data.table(key = "ea,age,start.year"),
#                    all.x = TRUE, by = c("ea", "age","start.year")) %>% as.data.frame
# 
# 
# liab.term %<>% as.data.frame %>%
#   group_by(start.year, ea, age.term) %>%
#   mutate(year.term = year[age == age.term],
#          
#          COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
#          ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#             
# 
#          B.v   = ifelse(age >= r.vben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.vben], 0),  # Benefit payment after r.vben
#          ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term)] * ax.vben[age == r.vben] * px_r.vben_m * v^(r.vben - age),
#                         B.v * ax.vben)
# 
#   ) %>%
#   ungroup  %>%
#   # select(#-start.year, -age.term,
#   #        -Bx.v, -ax.r.W, -COLA.scale, -pxRm) %>%
#   select(ea, age, start.year, year, year.term, B.v, ALx.v, ax.vben) %>%
#   # select(-age.term, -Bx.v, -ax.r.W.term, -COLA.scale, -pxRm, - px_r.vben_m, -age.r, -px_r.vsuper_m, -ax.vben, -pxm.term) %>%
#   filter(year %in% seq(init.year, len = nyear)) 
# 
# 
# # liab.term %<>% mutate(B.v   = ifelse(year.term == init.year - 1, 0, B.v),
# #                       ALx.v = ifelse(year.term == init.year - 1, 0, ALx.v))
# 
# # liab.term %>% filter(start.year ==2016, ea == 30, year.term == 2025) %>% 
# #   select(start.year, ea, year.term, age, year, B.v, ALx.v, ax.vben,  px_r.vben_m, pxm.term)
# 
# 
# 
# liab.term <-  bind_rows(list(liab.term.init,                                  # Using rbind produces duplicated rows with unknown reasons. Use bind_rows from dplyr instead.
#                              filter(liab.term, year.term != init.year - 1)))


# liab.term %>% filter(year.term == 2014, start.year == 1980) %>% head
# liab.term[!duplicated(liab.term %>% select(start.year, ea, age, year.term)),]
#   any(T)


liab_term <- expand.grid(# start.year   = (init.year - (r.vben - 1 - min.age)):(init.year + nyear - 1), # 2015
                         start_year   = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
                         ea = range_ea[range_ea < max_retAge],
                         age = range_age,
                         age_term = range_age[range_age < max_retAge]) %>%
  filter(start_year + max_age - ea >= init_year,
         age >= ea, 
  			 age_term >= ea,
         age >= age_term) %>% 
	mutate(B.v = 0, ALx.v = 0)

# #*************************************************************************************************************
# #                        4.1  ALs and NCs of benefit for death before retirement, for actives                  #####                  
# #*************************************************************************************************************
# # QSS: qualified Surviving Spouse
# # pct.QSS <- pct.ca.F * pct.female + pct.ca.M * pct.male
# # pct.QSS
# 
# 
# 
# # Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
# liab.active %<>%   
#   mutate( gx.death  = 0,
#           
#           Bx.death  = gx.death * yos * bfactor * fas, # annuity that would have been effective if the member retired on the 
#           
#           # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
#           # For PSERS: 1. Lump sum death benefit equal to PV of future benefit (Bx.death * ax.deathBen);
#           #            2. Death benefit are assumed to be claimed 1 year after death      
#           TCx.death = 0, #qxm.pre * v * lead(Bx.death) *  lead(ax.deathBen), # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
# 
#           # TCx.r = Bx.r * qxr.a * ax,
#           PVFBx.death  = 0,#  c(get_PVFB(pxT[age <= r.max], v, TCx.death[age <= r.max]), rep(0, max.age - r.max)),
#           
#           ## NC and AL of UC
#           # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
#           # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
#           # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
#           
#           # # NC and AL of PUC
#           # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
#           # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
#           # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
#           
#           
#           ## Under EAN methods, costs are spread up to r.max
#           # NC and AL of EAN.CD
#           NCx.EAN.CD.death = 0, #ifelse(age < r.max, PVFBx.death[age == min(age)]/ayx[age == r.max], 0),
#           ALx.EAN.CD.death = 0, #PVFBx.death - NCx.EAN.CD.death * axR,
#           
#           # NC and AL of EAN.CP
#           NCx.EAN.CP.death   = 0, #ifelse(age < r.max, sx * PVFBx.death[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
#           PVFNC.EAN.CP.death = 0, #NCx.EAN.CP.death * axRs,
#           ALx.EAN.CP.death   = 0 #PVFBx.death - PVFNC.EAN.CP.death
#   ) 
# 
# 
# 
# #*************************************************************************************************************
# #                       4.2   ALs and benefits for QSS for death benefit before retirement               #####                  
# #*************************************************************************************************************
# 
# # No death benefit for NC TSERS
# 
# liab.death <- rbind(
#   # grids for who die after year 1.
#   expand.grid(ea           = range_ea[range_ea < r.max],
#               age.death = min.age:r.max,
#               start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
#               age          = range_age) %>%
#     filter(age   >= ea,
#            age.death >= ea,
#            age   >= age.death,
#            start.year + (age.death - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
#            start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
# )
# 
# 
# # %>%
# # data.table(key = "start.year,ea,age.death,age")
# 
# liab.death %<>% mutate(B.death = 0, ALx.death = 0)
# 
# 
# 
# # liab.death <- merge(liab.death,
# #                  select(liab.active, start.year, ea, age, Bx.death, COLA.scale, gx.death, ax.deathBen, pxm.deathBen) %>% data.table(key = "ea,age,start.year"),
# #                  all.x = TRUE, 
# #                  by = c("ea", "age","start.year")) %>%
# #   arrange(start.year, ea, age.death) %>% 
# #   as.data.frame 
# #   #%>% 
# #   # left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
# #   #left_join(benefit_)
# 
# 
# # liab.death %<>% as.data.frame  %>% 
# #   group_by(start.year, ea, age.death) %>%
# #   mutate(
# #     
# #     # COLA.scale = (1 + cola)^(age - min(age)),         # COLA.scale in liab.active does not trace back long enough
# #     ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale), # COLA.scale in liab.active does not trace back long enough
# #     
# #     year       = start.year + age - ea,
# #     year.death = start.year + age.death - ea, # year of death of the active
# #     Bx.death   = ifelse(is.na(Bx.death), 0, Bx.death),  # just for safety
# #     
# #     # For PSERS: Lump sum death benefit equal to PV of future benefit. Benefit claim 1 year after death
# #     B.death    = ifelse(age == age.death + 1, Bx.death * ax.deathBen, 0),   # Bx.death[age == age.death] * COLA.scale / COLA.scale[age == age.death],               # Benefits for retirees after year 1
# #     ALx.death  = ifelse(age == age.death + 1, B.death, 0)                   # B.death * ax.deathBen                                                                # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
# #     
# #   ) %>% ungroup %>%
# #   # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
# #   filter(year %in% seq(init.year, len = nyear) ) %>%
# #   select(year, ea, age, year.death, age.death, start.year, B.death, ALx.death) %>% 
# #   arrange(age.death, start.year, ea, age)
# # 
# 
# # liab.death %>% ungroup %>% arrange(start.year, ea, year.death, age) %>%  head(100)
# 
# 
# 
# #*************************************************************************************************************
# #                        5.1  ALs and NCs of disability benefit, for actives                  #####                  
# #*************************************************************************************************************
# 
# 
# 
# # Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
# liab.active %<>%   
#   mutate( gx.disb  = 0,
#           Bx.disb  = gx.disb * bfactor * yos * fas, 
#           
#           # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
#           TCx.disb.la = qxd.la * v * lead(Bx.disb) *  lead(ax.disb.la) , # term cost of life annuity at the disability age x (start to claim benefit at age x + 1)
#           TCx.disb.ca = qxd.ca * v * lead(Bx.disb) *  lead(liab.disb.ca.sum.1),
#           TCx.disb.laca = TCx.disb.la + TCx.disb.ca,
#           
#           # TCx.r = Bx.r * qxr.a * ax,
#           PVFBx.disb  = 0,# c(get_PVFB(pxT[age <= r.max], v, TCx.disb.laca[age <= r.max]), rep(0, max.age - r.max)),
#           
#           ## NC and AL of UC
#           # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
#           # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
#           # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
#           
#           # # NC and AL of PUC
#           # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
#           # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
#           # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
#           
#           # NC and AL of EAN.CD
#           NCx.EAN.CD.disb = 0,#ifelse(age < r.max, PVFBx.disb[age == min(age)]/ayx[age == r.max], 0),
#           ALx.EAN.CD.disb = 0,#PVFBx.disb - NCx.EAN.CD.disb * axR,
#           
#           # NC and AL of EAN.CP
#           NCx.EAN.CP.disb   = 0,#ifelse(age < r.max, sx * PVFBx.disb[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
#           PVFNC.EAN.CP.disb = 0,#NCx.EAN.CP.disb * axRs,
#           ALx.EAN.CP.disb   = 0#PVFBx.disb - PVFNC.EAN.CP.disb
#   ) 
# 
# 
# 
# #*************************************************************************************************************
# #                       5.2   ALs and benefits for disability benefit               #####                  
# #*************************************************************************************************************
# 
# liab.disb.la <- rbind(
#   # grids for initial retirees in year 1
#   # To simplified the calculation, it is assmed that all initial disabled entered the workforce at age min.age and 
#   # become disabled in year 1. This assumption will cause the age of disability and yos of some of the disabled not compatible with the eligiblility rules,
#   # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial disabled.
#   expand.grid(age.disb   = benefit.disb_$age, # This ensures that year of retirement is year 1.
#               age        = range_age) %>%
#     mutate(ea            = unique(benefit.disb_$ea),
#            start.year    = init.year - (age.disb - ea)) %>% 
#     filter(age >= age.disb),
#   
#   # grids for who die after year 1.
#   expand.grid(ea           = range_ea[range_ea < r.max],
#               age.disb     = min.age:r.max,
#               start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
#               age          = range_age) %>%
#     filter(age   >= ea,
#            age.disb >= ea,
#            age   >= age.disb,
#            start.year + (age.disb - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
#            start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
# ) %>%
#   data.table(key = "start.year,ea,age.disb,age")
# 
# 
# # x <- liab.disb.la %>% mutate(year   = start.year + age - ea, year.disb = start.year + age.disb - ea) %>% filter(year == 2015, year.disb == 2015)
# # x
# # benefit.disb_%>% mutate(year   = start.year + age - ea, year.disb = start.year + age.disb - ea)
# 
# 
# 
# 
# liab.disb.la <- liab.disb.la[!duplicated(liab.disb.la %>% select(start.year, ea, age, age.disb ))]
# 
# 
# liab.disb.la <- merge(liab.disb.la,
#                     select(liab.active, start.year, ea, age, Bx.disb, COLA.scale, gx.disb, ax.disb.la) %>% data.table(key = "ea,age,start.year"),
#                     all.x = TRUE, 
#                     by = c("ea", "age","start.year")) %>%
#   arrange(start.year, ea, age.disb) %>% 
#   as.data.frame %>% 
#   left_join(benefit.disb_) %>% 
#   left_join(decrement.model_ %>% select(start.year, ea, age, pxm.d)) 
# 
# #%>% 
# # left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
# 
# #liab.disb.la %>% as.data.frame %>% mutate(year = start.year + age - ea) %>% 
#  # filter(year == 2015, age.disb == age)
# 
# liab.disb.la %<>% as.data.frame  %>% 
#   group_by(start.year, ea, age.disb) %>%
#   mutate(
#     year       = start.year + age - ea,
#     
#     COLA.scale = (1 + cola)^(age - min(age)),    # COLA.scale in liab.active does not trace back long enough
#     ax.disb.la = get_tla(pxm.d, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#     year.disb  = start.year + age.disb - ea,     # year of disability of the active
#     
#     Bx.disb    = ifelse(is.na(Bx.disb), 0, Bx.disb),  # just for safety
#     B.disb.la     = ifelse(year.disb <= init.year,
#                            benefit.disb[year == init.year] * COLA.scale / COLA.scale[year == init.year],  # Benefits for initial retirees
#                            Bx.disb[age == age.disb] * COLA.scale / COLA.scale[age == age.disb]),          # Benefits for disability retirees after year 1
#     ALx.disb.la   = B.disb.la * ax.disb.la                                                                # Liability for remaining diability benefits, PV of all future benefit adjusted with COLA
#     
#   ) %>% ungroup %>%
#   # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
#   filter(year %in% seq(init.year, len = nyear) ) %>%
#   select(year, ea, age, year.disb, age.disb, start.year, B.disb.la, ALx.disb.la) 
#   #%>%   arrange(age.disb, start.year, ea, age)
# 
# 
# # liab.disb %>% ungroup %>% arrange(start.year, ea, year.disb, age) %>%  head(100)
# 




#*************************************************************************************************************
#                 # 6.  Choosing AL and NC variables corresponding to the chosen acturial methed             #####
#*************************************************************************************************************


liab_active %<>% ungroup %>% select(start_year, year, ea, age, everything())


ALx.laca.method     <- paste0("ALx.", actuarial_method, ".laca")
NCx.laca.method     <- paste0("NCx.", actuarial_method, ".laca")
PVFNC.laca.method   <- paste0("PVFNC.", actuarial_method, ".laca")

# ALx.death.method   <- paste0("ALx.", actuarial_method, ".death")
# NCx.death.method   <- paste0("NCx.", actuarial_method, ".death")
# PVFNC.death.method <- paste0("PVFNC.", actuarial_method, ".death")
# 
# 
# ALx.disb.method     <- paste0("ALx.", actuarial_method, ".disb")
# NCx.disb.method     <- paste0("NCx.", actuarial_method, ".disb")
# PVFNC.disb.method   <- paste0("PVFNC.", actuarial_method, ".disb")

ALx.v.method    <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method    <- paste0("NCx.", actuarial_method, ".v")
PVFNC.v.method  <- paste0("PVFNC.", actuarial_method, ".v")


var.names <- c("sx", ALx.laca.method,  NCx.laca.method,  PVFNC.laca.method,
                     ALx.v.method,     NCx.v.method,     PVFNC.v.method,
                     #ALx.death.method, NCx.death.method, PVFNC.death.method,
                     #ALx.disb.method,  NCx.disb.method,  PVFNC.disb.method,
                     "PVFBx.laca", "PVFBx.v", 
							       #"PVFBx.death", "PVFBx.disb", 
							       #"Bx.laca", "Bx.disb", "Bx", 
							       "PVFSx")
liab_active %<>% 
  filter(year %in% seq(init_year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx.laca"   = ALx.laca.method,  "NCx.laca"   = NCx.laca.method,  "PVFNC.laca"   = PVFNC.laca.method, 
          "ALx.v"      = ALx.v.method,     "NCx.v"      = NCx.v.method,     "PVFNC.v"      = PVFNC.v.method
          #"ALx.death"  = ALx.death.method, "NCx.death"  = NCx.death.method, "PVFNC.death"  = PVFNC.death.method,
          #"ALx.disb"   = ALx.disb.method,  "NCx.disb"   = NCx.disb.method,  "PVFNC.disb"   = PVFNC.disb.method
          
           )   # Note that dplyr::rename_ is used. 




## Final outputs
  # liab.active
  # liab.la
  # liab.term
  # B.LSC

liab <- list(active = liab_active, 
             la = liab_la, 
             term = liab_term
             #death = liab.death, 
             #disb.la = liab.disb.la
						 )

}


# liab <- get_indivLab("t4a")


# liab <- get_indivLab(decrement.ucrp,
#                      salary,
#                      benefit,
#                      bfactor,
#                      init_terminated.t76)



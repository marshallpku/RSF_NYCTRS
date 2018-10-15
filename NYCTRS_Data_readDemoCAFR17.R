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



#*********************************************************************************************************
#                      ## Global settings  ####
#*********************************************************************************************************

dir_data  <- "Inputs_data/"



#*********************************************************************************************************
#                      ## 1. Loading data  ####
#*********************************************************************************************************


# NYSTRS 

df_NYCsurvey <- read_xlsx(paste0(dir_data, "Avg. Salary by Position-Age-Service.xlsx"), 
													sheet = "Table 1", 
													range = "A2:E122",
													col_names = c("position",
																				"agegrp",
																				"yosgrp",
																				"N",
																				"salary"))


df_NYCsurvey %<>% 
	mutate(agegrp_lb = as.numeric(str_extract(agegrp, "^\\d{2}")),
				 agegrp_ub = as.numeric(str_extract(agegrp, "\\d{2}$")),
				 
				 agegrp_lb = ifelse(agegrp_lb == 66,  65, agegrp_lb),
				 agegrp_ub = ifelse(is.na(agegrp_ub), 69, agegrp_ub),
				 
				 
				 yosgrp = str_replace(yosgrp, "'", ""),
				 yosgrp_lb = as.numeric(str_extract(yosgrp, "^\\d{1,}\\s")),
				 yosgrp_ub = as.numeric(str_extract(yosgrp, "\\s\\d{1,}$")),
				 
				 yosgrp_lb = ifelse(is.na(yosgrp_lb), 30, yosgrp_lb),
				 yosgrp_ub = ifelse(is.na(yosgrp_ub), 34, yosgrp_ub),
				 
				 age.cell = (agegrp_lb + agegrp_ub)/2,
				 yos.cell = (yosgrp_lb + yosgrp_ub)/2
				 ) %>% 
	select(position, agegrp, yosgrp, age.cell, yos.cell, N, salary)

df_NYCsurvey	


df_NYCsurvey %>% 
	ungroup() %>% 
	ggplot(aes(x = yos.cell, y = N, color = position)) + 
	facet_wrap(~age.cell, ncol = 4)+
	geom_line() +
	geom_point()


df_NYCsurvey %>% 
	ungroup() %>% 
	ggplot(aes(x = yos.cell, y = salary, color = position)) + 
	facet_wrap(~age.cell, ncol = 4)+
	geom_line() +
	geom_point()







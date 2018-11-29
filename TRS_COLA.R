# Explore how to best model TRS COLA

# COLA (same across all tiers):
# 	- Eligibility
#     Service retirees, beginning at age 62 and who have been retired for >= 5 years
#     Service retirees, beginning at age 55 and who have been retired for >= 10 years
# - Amount:
# 	  Between 1% and 3%, based on half of the CPI:   min(3%, max(1%, 0.5*CPI))
#     The increase will be calculated on the lesser of the allowance under the Maximum Payment Option or $1,8000 (How often does this number adjust?)


# Example data frame:

N <- 6

i <- 0.07
v <- 1/(1+0.07)

df <- data.frame(
	ea  = rep(20, 46),
	age = 55:100,
	px  = rep(0.99, 46),
	Bx  = c(seq(10000, 30000, len = 6), rep(0, 40))
)

df

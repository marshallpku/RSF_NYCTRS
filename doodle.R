
asset_tot_by17 <- 43629545 + 20292733
asset_TDA_by17 <- 20292733

earnings_tot17 <- 8115830  # 7327814 
earnings_TDA17 <- 2417743

TDA_fixpay <- 1466615

# return total
earnings_tot17 / asset_tot_by17 #12.70%

# return TDA
earnings_TDA17 / asset_TDA_by17


# QPP Return with TDA
(earnings_tot17 - TDA_fixpay) / (asset_tot_by17 - asset_TDA_by17) #13.4%

# QPP Return w/o TDA
(earnings_tot17 - earnings_TDA17) / (asset_tot_by17 - asset_TDA_by17) #11.25%


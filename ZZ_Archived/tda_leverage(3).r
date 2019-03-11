

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library("scales")


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************

pany <- function(x, p, na.rm=FALSE) {
  # function to compute any percentile
  # where x is the variable of interest, p is the percentile
  as.numeric(quantile(x, p, na.rm=na.rm))
}

sd.pop <- function(x) {
  # population standard deviation
  n <- length(x)
  sqrt((n-1)/n) * sd(x)
}

eror <- function(assets.eoy, assets.y1, nyears){
  # compute effective rate of return for each year and sim
  
  # assets.eoy is a matrix with end-of-year assets
  # where rows are sims, columns are years
  # assets.y1 is a scalar with year-1 beginning-of-year asset value, as that is not in assets.eoy
  # it is the same for all sims
  
  # we divide each year's ending assets by beginning-of-year (boy) assets and subtract 1
  assets.boy <- cbind(assets.y1, assets.eoy[, 1:(nyears-1)]) # insert year 1 value in first column, shift others to right
  eror <- assets.eoy / assets.boy - 1
  return(eror)
}


#****************************************************************************************************
#                Analysis ####
#****************************************************************************************************

nyears <- 30

# investment return assumptions
ir.mean <- .07
ir.sd <- .12

nsims <- 10e3

set.seed(1234) # so results are reproducible
ir.mat <- matrix(rnorm(nsims*nyears, mean=ir.mean, sd=ir.sd), nrow=nsims, ncol=nyears) # investment return matrix:  nsims x nyears -- all returns independent of each other
head(ir.mat)

# do we have the right mean and sd each year? yes
apply(ir.mat, 2, mean)
apply(ir.mat, 2, sd)
apply(ir.mat, 2, sd.pop)
apply(ir.mat, 2, min)
apply(ir.mat, 2, max)

# note that when seed=1234, sim 3238 goes wild around year 26
ir.mat[3238, 20:30] # years 21:24 are negative, 25 is low, and 26 is negative

# end of year (eoy) accumulation factors given these returns
accum.eoy <- t(apply(ir.mat + 1, 1, cumprod))
head(accum.eoy)
apply(accum.eoy, 2, min)
apply(accum.eoy, 2, max)

# RERUN from here as needed, with different TDA guarantees and initial shares ----
# define year 1 assets
assets.qpp.y1 <- 1
assets.tda.y1 <- .4
assets.tot.y1 <- assets.qpp.y1 + assets.tda.y1

# total assets at end of each year
assets.tot.eoy <- assets.tot.y1 * accum.eoy
head(assets.tot.eoy)

# qpp assets at end of each year, no tda guarantee
assets.qpp_notda.eoy <- assets.qpp.y1 * accum.eoy
head(assets.qpp_notda.eoy)

# qpp assets at end of each year IF ir.qpp.mean is achieved every year
assets.qpp_assumed.eoy <- assets.qpp.y1 * (1 + ir.qpp.mean)^(1:nyears)
head(assets.qpp_assumed.eoy)

# compute the tda guarantee
tdarate <- .07  # the guaranteed rate -- need not be same as ir.qpp.mean
assets.tda.eoy <- assets.tda.y1 * (1 + tdarate)^(1:nyears)
assets.tda.eoy

# subtract assets.tda.eoy from each row of assets.tot.eoy, since it is fixed
assets.qpp_tda.eoy <- sweep(assets.tot.eoy, 2, assets.tda.eoy)

head(assets.tot.eoy[, 1:8])
assets.tda.eoy[1:8] 
head(assets.qpp_tda.eoy[, 1:8])
head(assets.qpp_notda.eoy[, 1:8])

# compute effective rate of return on qpp assets with and without tda guarantee
eror.qpp_notda <- eror(assets.qpp_notda.eoy, assets.qpp.y1, nyears)
eror.qpp_tda <- eror(assets.qpp_tda.eoy, assets.qpp.y1, nyears)

# check
head(ir.mat) # the actual returns
head(eror.qpp_notda[, 1:8]) # returns without tda should be the same as raw returns
head(eror.qpp_tda[, 1:8]) # returns with tda should be more variable

#..start with simple analysis of the mean and sd of returns ----
# create a data frame of annual returns
df1 <- as_tibble(eror.qpp_notda) %>%
  setNames(., paste0("y", 1:nyears)) %>%
  mutate(type="notda", sim=row_number())
glimpse(df1)
df2 <- as_tibble(eror.qpp_tda) %>%
  setNames(., paste0("y", 1:nyears)) %>%
  mutate(type="tda", sim=row_number())
df <- bind_rows(df1, df2) %>%
  gather(year, eror, -type, -sim) %>%
  mutate(year=as.integer(str_sub(year, 2, -1)))
glimpse(df)

df.stats <- df %>%
  group_by(type, year) %>%
  summarise(eror.mdn=median(eror),
            eror.mean=mean(eror),
            eror.sd=sd.pop(eror),
            eror.iqr=IQR(eror))


fname <- function(var, stat, share){
  # function to build file name for graphs to save
  fname <- paste0("./", var, ".", stat, ".share", share, ".png")
  fname
}

eror.mdn <- df.stats %>%
  ggplot(aes(year, eror.mdn, colour=type)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = ir.mean) +
  ggtitle("Median QPP effective rates of return, with and without TDA guarantee",
          subtitle=paste0("Year 1 TDA is ", assets.tda.y1, " of year 1 QPP"))
eror.mdn
# eror median has higher highs, lower lows, when we have tda 

ggsave(fname("eror", "mdn", assets.tda.y1), eror.mdn)

df.stats %>%
  ggplot(aes(year, eror.mean, colour=type)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = ir.mean)
# means can produce wild numbers

# let's find out why sim 3238 when seed=1234 gives such wild values in years 25 and 26
irows <- 3238
icols <- 20:27
assets.tot.eoy[irows, icols] %>% round(3)
assets.tda.eoy[icols] %>% round(3)
assets.qpp_tda.eoy[irows, icols] %>% round(3)
# at the extreme, the tda can be bigger than total assets
# so median is a better way to see what is typical

df.stats %>%
  ggplot(aes(year, eror.sd, colour=type)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = ir.sd)
# also the sd gets quite wild

eror.iqr <- df.stats %>%
  ggplot(aes(year, eror.iqr, colour=type)) + 
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits=c(0, NA)) +
  geom_hline(yintercept = 0) +
  ggtitle("Interquartile range for QPP effective rates of return, with and without TDA guarantee",
          subtitle=paste0("Year 1 TDA is ", assets.tda.y1, " of year 1 QPP"))
eror.iqr
# iqr is much larger for the tda, and increases over time
# eror median has higher highs, lower lows, when we have tda
ggsave(fname("eror", "iqr", assets.tda.y1), eror.iqr)

# find the wild numbers
wild <- df %>%
  filter(type=="tda", year==26)
quantile(wild$eror, probs=c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
# sim 3238 is the weird one

#..now see what happens to asset accumulations ----
dfa.assume <- as.tibble(assets.qpp_assumed.eoy) %>%
  mutate(year=row_number() %>% as.integer) %>%
  select(year, assume=value)

dfa.notda <- as_tibble(assets.qpp_notda.eoy) %>%
  setNames(., paste0("y", 1:nyears)) %>%
  mutate(type="notda", sim=row_number())

dfa.tda <- as_tibble(assets.qpp_tda.eoy) %>%
  setNames(., paste0("y", 1:nyears)) %>%
  mutate(type="tda", sim=row_number())

dfa <- bind_rows(dfa.notda, dfa.tda) %>%
  gather(year, assets, -type, -sim) %>%
  mutate(year=as.integer(str_sub(year, 2, -1))) %>%
  left_join(dfa.assume) %>%
  mutate(iassets=assets / assume)
glimpse(dfa)

dfa.stats <- dfa %>%
  group_by(year, type) %>%
  summarise(iassets.mdn=median(iassets),
            iassets.p25=pany(iassets, .25),
            iassets.p75=pany(iassets, .75))

a.mdn <- dfa.stats %>%
  ggplot(aes(year, iassets.mdn, colour=type)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=seq(0, 1, .05)) +
  ggtitle("Median of QPP assets divided by assumed QPP assets, with and without TDA guarantee",
          subtitle=paste0("Year 1 TDA is ", assets.tda.y1, " of year 1 QPP"))
a.mdn
ggsave(fname("iassets", "mdn", assets.tda.y1), a.mdn)


a.p25 <- dfa.stats %>%
  ggplot(aes(year, iassets.p25, colour=type)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=seq(0, 3, .05)) +
  ggtitle("P25 of QPP assets divided by assumed QPP assets, with and without TDA guarantee",
          subtitle=paste0("Year 1 TDA is ", assets.tda.y1, " of year 1 QPP"))
a.p25
ggsave(fname("iassets", "p25", assets.tda.y1), a.p25)

a.p75 <- dfa.stats %>%
  ggplot(aes(year, iassets.p75, colour=type)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=seq(0, 3, .05)) +
  ggtitle("P75 of QPP assets divided by assumed QPP assets, with and without TDA guarantee",
          subtitle=paste0("Year 1 TDA is ", assets.tda.y1, " of year 1 QPP"))
a.p75
ggsave(fname("iassets", "p75", assets.tda.y1), a.p75)



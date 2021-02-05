library(rio)
library(stargazer)
library(tidyverse)
library(AER)
library(plm)

# Problem 1: it's not consistent

# Problem 2
dfM <- import("Data/marriagemarket.dta")

# Drop NA's
dfM <- drop_na(dfM, "mortality")

# I) Regress births on pre-war sex ratio
lm_pre <- lm(illeg ~ sr, dfM[dfM$post == 0,])


# function: calcs robust se for lm() object
calc_robust_se <- function(model, clustered){
  if (clustered==TRUE){
    cov_model <- vcovHC(model, type = "HC1", cluster="group")
    robust_se <- sqrt(diag(cov_model))
    return(robust_se)
  }
  else{
    cov_model <- vcovHC(model, type = "HC1")
    robust_se <- sqrt(diag(cov_model))
    return(robust_se)
  }
}  

# calc robust se for first regression
robust_se_lm_pre <- calc_robust_se(lm_pre, F)

# create output table
stargazer(lm_pre, 
          #type = "text", 
          se = list(robust_se_lm_pre),
          keep.stat=c("n","adj.rsq"))

# II) Generate a dummy for whether the military mortality in a region is above thee median
dfM$mortality_above <- ifelse(dfM$mortality > median(dfM$mortality), 1, 0)

# Create a dummy for 4 groups: post and pre war + above and below median mortality
dfM$groups <- ifelse(dfM$post == 1 & dfM$mortality_above == 1, "Post_above", ifelse(dfM$post & dfM$mortality_above == 0, "Post_below", ifelse(dfM$mortality_above == 1, "Pre_above", "Pre_below")))
dfM0 <- dfM[dfM$post == 0,]
dfM1 <- dfM[dfM$post == 1,]
dfM0$dif <- dfM1$illeg - dfM0$illeg 
dif_dif <- lm(dif ~ mortality, dfM0)

dfM %>%
  group_by(groups) %>%
  summarise(Births = mean(illeg))

DID <- mean(dfM[dfM$groups == "Post_above",]$illeg) - mean(dfM[dfM$groups == "Pre_above",]$illeg) - (mean(dfM[dfM$groups == "Post_below",]$illeg) - mean(dfM[dfM$groups == "Pre_below",]$illeg))
DID
# III) Estimate DID regression
# Create a dummy for post*mortality
dfM$post_mortality <- dfM$post * dfM$mortality

# interaction effect - no dummies 
lm_DID <- lm(illeg ~ post_mortality + post, dfM)

lm_DID_depc <- lm(illeg ~ post_mortality + post + as.factor(depc), dfM)

robust_se_lm_DID <- calc_robust_se(lm_DID, T)
robust_se_lm_DID_depc <-  calc_robust_se(lm_DID_depc, F)

stargazer(lm_DID, lm_DID_depc,
          #se = list(robust_se_lm_DID, robust_se_lm_DID_depc),
          type = 'latex',
          omit="depc",
          keep.stat=c("n","adj.rsq"))
getwd()
# IV) Key assumption? Paralell trends. We can't estimate as we have only 2 periods




library(rio)
library(stargazer)
library(tidyverse)

# Problem 1: it's not consistent

# Problem 2
setwd("~/Documents/Econometrics-II")
dfM <- import("Data/marriagemarket.dta")

# Drop NA's
dfM <- drop_na(dfM, "mortality")

# I) Regress births on pre-war sex ratio
lm_pre <- lm(illeg ~ sr, dfM[dfM$post == 0,])

stargazer(lm_pre, type = "text", 
          keep.stat=c("n","adj.rsq"))

# II) Generate a dummy for whether the military mortality in a region is above thee median
dfM$mortality_above <- ifelse(dfM$mortality > median(dfM$mortality), 1, 0)

# Create a dummy for 4 groups: post and pre war + above and below median mortality
dfM$groups <- ifelse(dfM$post == 1 & dfM$mortality_above == 1, "Post_above", ifelse(dfM$post & dfM$mortality_above == 0, "Post_below", ifelse(dfM$mortality_above == 1, "Pre_above", "Pre_below")))

dfM %>%
  group_by(groups) %>%
  summarise(Births = mean(illeg))

DID <- mean(dfM[dfM$groups == "Post_above",]$illeg) - mean(dfM[dfM$groups == "Pre_above",]$illeg) - (mean(dfM[dfM$groups == "Post_below",]$illeg) - mean(dfM[dfM$groups == "Pre_below",]$illeg))
DID

# III) Estimate DID regression
# Create a dummy for post*mortality
dfM$post_mortality <- dfM$post * dfM$mortality

lm_DID <- lm(illeg ~ post_mortality + post, dfM)
lm_DID_depc <- lm(illeg ~ post_mortality + post + as.factor(depc), dfM)
stargazer(lm_DID, lm_DID_depc, type = "text", 
          keep.stat=c("n","adj.rsq"))

# IV) Key assumption? Paralell trends. We can't estimate as we have only 2 periods






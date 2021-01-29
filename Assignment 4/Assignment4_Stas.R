library(rio)
library(tidyverse)
library(AER)
#============== Use rio package to import data
#-------------------------
# Problem 1
# dataframe with the percentages per case/arrest and judge
dfJudge <- data.frame( cases = c(0.7, 0.3, 0.4, 0.6), arrests = c(0.4, 0.6, 0.2, 0.5) )
rownames(dfJudge) <- c("Jones-Prison", "Jones-Other", "Smith-Prison", "Smith-Other")

PY_1_Z_1 = dfJudge[1,1]*dfJudge[1,2] + dfJudge[2,1]*dfJudge[2,2]
PY_1_Z_0 = dfJudge[3,1]*dfJudge[3,2] + dfJudge[4,1]*dfJudge[4,2]
PD_1_Z_1 = dfJudge[1,1]
PD_1_Z_0 = dfJudge[3,1]

wald_est <- (PY_1_Z_1 - PY_1_Z_0) / (PD_1_Z_1 - PD_1_Z_0)
wald_est

#---------------------------
# Problem 2
# (i): determine size
round((((1.96 + 0.524)/0.1)^2) * (0.5*0.5)/(0.5*(1 - 0.5)), 0)

#-----------------------------
# (ii): determine size with non-compliers
round((((1.96 + 0.524)/(0.8*0.1))^2) * (0.5*0.5)/(0.5*(1 - 0.5)), 0)
#========== I get different sizes (I trust my results more)

#------------------------
# Problem 3
setwd("~/Documents/Econometrics-II")
dfFlu <- import("Data/FluData.dta")
dfFlu_treatment <- dfFlu[dfFlu$TreatGroup == 1,]
dfFlu_control <- dfFlu[dfFlu$TreatGroup == 0,]

# (i) Calculate variance for the control group AND size
p <- sum(dfFlu_control$Flu)/nrow(dfFlu_control)
sigma2 <- p*(1-p)
print(p)
print(sigma2)
round((((1.96 + 0.84)/0.05)^2) * (sigma2)/(0.8*(1 - 0.8)), 0)
#======== We need to use CONTROL and not TREATMENT group

# (ii) Fraction of actual recevers of the flu shot
p_actual <- sum(dfFlu_treatment$Treatment)/nrow(dfFlu_treatment)
round((((1.96 + 0.84)/(p_actual*0.05))^2) * (sigma2)/(0.8*(1 - 0.8)), 0)
#========== I get different sizes

#-------------------------------
# (iii) Make a summary table
dfFlu$status <- ifelse(dfFlu$TreatGroup == 0, "Control", ifelse(dfFlu$Treatment == 1, "Treated", "Untreated"))
#======== COULD be done easier with ifelse

dfFlu %>%
  group_by(status) %>%
  summarise(Share = n()/12583,
            Gender = mean(GenderChild),
            Mother_Age = mean(AgeMother),
            Mother_Edu = mean(EducationMother),
            Marrage = mean(Married),
            Nation = mean(Nationality),
            Income = mean(Hhincome))
#========== We can include a share of total in the table

#------------------------------
# (iv) OLS with all randomized children
lm_all <- lm(Flu ~ Treatment, dfFlu[dfFlu$TreatGroup == 1,])
lm_all_all <- lm(Flu ~ Treatment + GenderChild + AgeMother + Married + Nationality + Hhincome, dfFlu[dfFlu$TreatGroup == 1,])
#============= Should we use only children assigned to the treatment group? I did here
#============= Should check robust s.e.

stargazer(lm_all, lm_all_all,
          type="text", report="vc*stp",
          keep.stat=c("n","adj.rsq"),
          se = list(sqrt(diag(vcovHC(lm_all, type = "HC1"))), sqrt(diag(vcovHC(lm_all_all, type = "HC1")))),
          title = "The effect of flu vaccine on the probability to get flu in the treatment group (ATET)")

#---------------------------
# (v) Estimate IV model with all children
iv_all <- ivreg(Flu ~ Treatment | TreatGroup, data = dfFlu)
iv_all_all <- ivreg(Flu ~ Treatment + GenderChild + AgeMother + Married + Nationality + Hhincome | TreatGroup + GenderChild + AgeMother + Married + Nationality + Hhincome, data = dfFlu)

stargazer(iv_all, iv_all_all,
          type="text", report="vc*stp",
          keep.stat=c("n","adj.rsq"),
          se = list(sqrt(diag(vcovHC(iv_all, type = "HC1"))), sqrt(diag(vcovHC(iv_all_all, type = "HC1")))),
          title = "The effect of flu vaccine on the probability to get flu with IV (ATE)")
#============ Same question with robust s.e.

#-------------------------
# (vi)
lm_shot <- lm(Treatment ~ TreatGroup, dfFlu)
lm_shot_all <- lm(Treatment ~ TreatGroup + GenderChild + AgeMother + Married + Nationality + Hhincome, dfFlu)
stargazer(lm_shot, lm_shot_all,
          type="text", report="vc*stp",
          keep.stat=c("n","adj.rsq", "f"),
          se = list(sqrt(diag(vcovHC(lm_shot, type = "HC1"))), sqrt(diag(vcovHC(lm_shot_all, type = "HC1")))),
          title = "The effect of being assigned to a treatment group on the probability to get flu vaccine")
#============= Should we use F-test = 10 only?

# (vii)
# Because non-compliers have not optted out by their will rather they couldn't receive it due to other reasons (not individual characteristics)



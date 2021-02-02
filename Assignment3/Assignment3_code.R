# Packes required for subsequent analysis. P_load ensures these will be installed and loaded. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plm, 
               Formula,
               car,
               clubSandwich,
               lmtest,
               foreign, 
               tidyverse)


# create df with treatment effects
dfTreatment <- data.frame(N_treated = c(100, 75, 25), N_control = c(100, 25, 75), Avg_Outcome_treated = c(9, 13,10), Avg_Outcome_control = c(7,8,9))
row.names(dfTreatment) <- c("Purple", "Blue", "Green")

# average treatment effect per group
ATE_group <- dfTreatment$Avg_Outcome_treated - dfTreatment$Avg_Outcome_control

ATE_treated <- sum(dfTreatment$N_treated/sum(dfTreatment$N_treated) * dfTreatment$Avg_Outcome_treated)
ATE_control <- sum(dfTreatment$N_control/sum(dfTreatment$N_control) * dfTreatment$Avg_Outcome_control)
ATE_treated - ATE_control



dfBonus <- read.dta("Data/bonus.dta")

dfBonus$category <- ifelse(dfBonus$bonus500 == 1, "Low-reward", ifelse(dfBonus$bonus1500 == 1, "High-reward", "Control"))

dfBonus %>% 
  group_by(category) %>% 
  summarise(perc_passed_year1 = sum(pass)/ n(), 
            avg_myeduc = mean(myeduc),
            avg_fyeduc = mean(fyeduc), 
            avg_p0 = mean(p0),
            math = mean(math[!is.na(math)]),
            perc_job = sum(job[!is.na(job)])/n(),
            avg_effort = mean(effort[!is.na(effort)]),
            total = n())



LPM_simple <- lm(pass ~ category, data=dfBonus)
LPM_addedRegressors <- lm(pass ~ category + math + fyeduc + p0, data=dfBonus)
LPM_allRegressors <- lm(pass ~ category + math + fyeduc + p0 + effort + job, data=dfBonus)


mX <- dfBonus %>% select(math, fyeduc, p0, effort, job) %>% as.matrix() %>% na.omit()
cor(mX)
vif(LPM_allRegressors)

summary(LPM_simple)
summary(LPM_addedRegressors)
summary(LPM_allRegressors)

LPM_drop <- lm(dropout ~ category + math + fyeduc + p0 + effort + job, data=dfBonus)
LM_pointsYear1 <- lm(stp2001 ~ category + math + fyeduc + p0 + effort + job, data=dfBonus)
LM_pointsYear3 <- lm(stp2004 ~ category + math + fyeduc + p0 + effort + job, data=dfBonus)

summary(LPM_drop)
summary(LM_pointsYear1)
summary(LM_pointsYear3)


# define power and alpha
power <- 0.8
alpha <- 0.05

# define model for only low, and only high

qnorm(1-alpha/2, 0,1)

dfBonus$bonus500

LM_all_low <- lm(pass ~ bonus500 + math + fyeduc + p0 + effort + job, data=dfBonus %>% filter(bonus1500 != 1))
LM_all_high <- lm(pass ~ bonus1500 + math + fyeduc + p0 + effort + job, data=dfBonus %>% filter(bonus500 != 1))


get_MDE <- function(Model_exp, sGroup, fAlpha,fPower){
  
  # get data used in the model
  dfExp <- Model_exp$model
  
  # get the n for the model
  n <- nrow(dfExp)
  
  # count how many in treatment
  nTreatment <- sum(dfExp[, sGroup])
  
  # get proportion of control
  p <- (n-nTreatment)/n
  
  # get degrees of freedom
  iDF <- length(Model_exp$coefficients)
  
  # get appropriate t-values
  t_alpha <- qt(1-fAlpha/2, df = iDF)
  t_q <- qt(1-fPower, df = iDF)
  
  # get variance of residuals
  Sigma2 <- var(Model_exp$residuals)
  
  # get the MDE
  MDE <- (t_alpha - t_q) * sqrt(1/(p*(1-p)))* sqrt(Sigma2/n)
  
  return(MDE)
  
}




MDE_low <- get_MDE(LM_all_low, "bonus500", alpha, power)
MDE_high <- get_MDE(LM_all_high, "bonus1500", alpha, power)  

get_n_givenMDE <- function(Model_exp, sGroup, fAlpha, fPower, MDE, p, Sigma2=1){
  
  # get t values
  t_alpha <- qnorm(1-fAlpha/2, 0,Sigma2)
  t_q <-qnorm(1-fPower, 0,Sigma2)

  # get variance of residuals
  z <- (1/sqrt(p*(1-p)))*(1/(t_alpha - t_q))

  n <- Sigma2/(z*MDE)^2
  
  return(n)
  
  
}

n_low <- get_n_givenMDE(LM_all_low, "bonus500", alpha, power, 0.1, 0.5)






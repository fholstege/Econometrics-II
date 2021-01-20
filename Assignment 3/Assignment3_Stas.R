pacman::p_load(tidyverse,
               rio,
               skimr, 
               stargazer,
               erer)

dfBonus <- import("Data/bonus.dta")
dfBonus$category <- ifelse(dfBonus$bonus500 == 1, "Low-reward", ifelse(dfBonus$bonus1500 == 1, "High-reward", "Control"))

sum(dfBonus[dfBonus$category == "Low-reward",]$job, na.rm=T)/sum(!is.na(dfBonus[dfBonus$category == "Low-reward",]$job))
sum(dfBonus[dfBonus$category == "Low-reward",]$job, na.rm=T)/length(!is.na(dfBonus[dfBonus$category == "Low-reward",]$job))
# we shouldn't include NA's when calculating the means

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

lm_1 <- lm(pass ~ category, dfBonus)
lm_2 <- lm(pass ~ category + fyeduc + math + p0, dfBonus)
lm_3 <- lm(pass ~ category + fyeduc + math + p0 + effort + job, dfBonus)
stargazer(lm_1, lm_2, lm_3,
          type = "text")

# (iv)
lm_drop <- lm(dropout ~ category, dfBonus)
lm_drop_2 <- lm(dropout ~ category + fyeduc + math + p0 + effort + job, dfBonus)
lm_drop_L <- glm(dropout ~ category, dfBonus, 
             family = binomial(link = "logit"), x = TRUE)
lm_drop_2L <- glm(dropout ~ category + fyeduc + math + p0 + effort + job, dfBonus, 
             family = binomial(link = "logit"), x = TRUE)
m1L <- maBina(lm_drop_L, x.mean = FALSE)
m2L <- maBina(lm_drop_2L, x.mean = FALSE)
# we could report logit model as well

stargazer(lm_drop, lm_drop_2, m1L, m2L,
          keep.stat=c("n","adj.rsq"),
          type = "text")

lm_pointsYear1 <- lm(stp2001 ~ category, dfBonus)
lm_pointsYear1_2 <- lm(stp2001 ~ category + fyeduc + math + p0 + effort + job, dfBonus)
lm_pointsYear3 <- lm(stp2004 ~ category, dfBonus)
lm_pointsYear3_2 <- lm(stp2004 ~ category + fyeduc + math + p0 + effort + job, dfBonus)

stargazer(lm_pointsYear1, lm_pointsYear1_2, lm_pointsYear3, lm_pointsYear3_2,
          keep.stat=c("n","adj.rsq"),
          type = "text")

#------------------------------------------
# power 

lm_points_high <- lm(dropout ~ bonus1500, dfBonus[dfBonus$category != "Low-reward",])
lm_points_low <- lm(dropout ~ bonus500, dfBonus[dfBonus$category != "High-reward",])

get_MDE <- function(lm_model, group, alpha, power){
  # Get the data
  dfModel <- lm_model$model 
  
  # N of coefficients
  n_coef <- length(lm_model$coefficients)
  
  # N of observations
  n <- nrow(dfModel)
  
  # N of treated
  nTreatment <- sum(dfModel[,group])
  
  # Get the share of treated
  p <- nTreatment/n
  
  # Get t-value
  t_alpha <- qt(1-alpha/2, n - n_coef)
  t_q <- qt(1-power, n - n_coef)
  
  # get variance of residuals
  sigma2 <- var(lm_model$residuals)
  
  # get the MDE
  MDE <- (t_alpha - t_q) * sqrt(1/(p*(1-p))) * sqrt(sigma2/n)
  
  return(MDE)
}

get_MDE(lm_points_high, "bonus1500", 0.05, 0.8)
get_MDE(lm_points_low, "bonus500", 0.05, 0.8)











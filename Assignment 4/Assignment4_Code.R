
# packages
library(readstata13)
library(AER)

# judges df
dfJudge <- data.frame( perc_sentence = c(0.7, 0.3, 0.4, 0.6), perc_arrest = c(0.4, 0.6, 0.2, 0.5) )
rownames(dfJudge) <- c("Jones-Prison", "Jones-Other", "Smith-Prison", "Smith-Other")

dfJudge$perc_arrest_cond = dfJudge$perc_sentence * dfJudge$perc_arrest


PY_1_Z_1 = sum(dfJudge$perc_arrest_cond[c(1,2)])
PY_1_Z_0 = sum(dfJudge$perc_arrest_cond[c(3,4)])
PD_1_Z_1 = dfJudge$perc_sentence[1]
PD_1_Z_0 = dfJudge$perc_sentence[3]


wald_est <- (PY_1_Z_1 - PY_1_Z_0)/ (PD_1_Z_1 - PD_1_Z_0)
wald_est



get_size_givenMDE <- function(MDE, fAlpha, fPower, p, Sigma2){
  
  # get t values
  t_alpha <-1.96
  t_q <- -0.84
  
  # get the MDE
  size <- (((t_alpha - t_q)/MDE)^2) * sigma2/(p*(1-p))
  size <- round(size, 0)
  
  return(size)
}
  

MDE = 0.1
fAlpha = 0.05
fPower = 0.7
p = 0.5
sigma2 <- p* (1- p)

size = get_size_givenMDE(MDE, fAlpha, fPower, p, sigma2)
size

perc_nonComply <- 0.2

new_size = (1/(1-perc_nonComply)^2)*size
new_size


## 3
setwd( "C:/Users/flori/OneDrive/Documents/Tinbergen/Courses/Econometrics II")
dfFlu <- read.dta13("Data/FluData.dta")
dfFlu_treatment <- dfFlu[dfFlu$TreatGroup == 1,]
dfFlu_control <- dfFlu[dfFlu$TreatGroup == 0,]


sigma2_flu_control <- p_flu*(1-p_flu)


size_flu <- get_size_givenMDE(0.05, fAlpha, fPower_flu, 0.8, sigma2_flu_control)
size_flu


dfFlu_actualTreatment <- dfFlu_treatment[dfFlu_treatment$Treatment == 1,]
perc_comply_flu <- nrow(dfFlu_actualTreatment)/nrow(dfFlu_treatment)


new_size_flu = (1/(perc_comply_flu)^2)*size_flu
new_size_flu


type_group <- ifelse(dfFlu$TreatGroup ==1 & dfFlu$Treatment == 0, "Untreated treatment",ifelse(dfFlu$TreatGroup ==1 & dfFlu$Treatment == 1, "Treated treatment","Control"))
dfFlu$type_group <- type_group

dfSummary <- dfFlu %>%
  group_by(type_group) %>% 
  summarise(perc_boy = sum(GenderChild)/n(),
            mean_age_mother = mean(AgeMother),
            mean_edu_mother = mean(EducationMother),
            perc_married = sum(Married)/n(),
            perc_nationality = sum(Nationality)/n(),
            mean_HHincome = mean(Hhincome),
            perc_group = n()/nrow(dfFlu)
            )
dfSummary
    

model_ols_simple <- lm(Flu ~ Treatment, data=dfFlu_treatment)
model_ols_extensive <- lm(Flu ~ Treatment + GenderChild + AgeMother + EducationMother + Married + Nationality + Hhincome, data = dfFlu_treatment)

summary(model_ols_simple)
summary(model_ols_extensive)
dfFlu$TreatGroup

model_ols_simple_iv <- ivreg(Flu ~ Treatment | TreatGroup, data=dfFlu)
model_ols_extensive_iv <- ivreg(Flu ~ Treatment + GenderChild + AgeMother + EducationMother + Married + Nationality + Hhincome
                                | TreatGroup + GenderChild + AgeMother + EducationMother + Married + Nationality + Hhincome, data=dfFlu)


summary(model_ols_simple_iv)
summary(model_ols_extensive_iv)

Partial_ols_FluShot <- lm(Treatment ~ TreatGroup, data=dfFlu)
summary(Partial_ols_FluShot)

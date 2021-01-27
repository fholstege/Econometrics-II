
install.packages("readstata13")
library(readstata13)


dfJudge <- data.frame( cases = c(0.7, 0.3, 0.4, 0.6), arrests = c(0.4, 0.6, 0.2, 0.5) )
rownames(dfJudge) <- c("Jones-Prison", "Jones-Other", "Smith-Prison", "Smith-Other")

wald_est <- (dfJudge$cases[1]- dfJudge$cases[3])/(dfJudge$arrests[1] - dfJudge$arrests[3])


get_size_givenMDE <- function(MDE, fAlpha, fPower, p, Sigma2){
  
  # get t values
  t_alpha <- qnorm(1-fAlpha/2, 0,Sigma2)
  t_q <-qnorm(1-fPower, 0,Sigma2)
  
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

perc_nonComply <- 0.2

new_size = (1/(1-perc_nonComply)^2)*size
new_size



dfFlu <- read.dta13("Data/FluData.dta")
dfFlu_treatment <- dfFlu[dfFlu$TreatGroup == 1,]
dfFlu_control <- dfFlu[dfFlu$TreatGroup == 0,]


p_flu <- sum(dfFlu_treatment$Flu)/nrow(dfFlu_treatment)
sigma2_flu <- p*(1-p)



size_flu <- get_size_givenMDE(0.05, fAlpha, fPower, p_flu, sigma2_flu)


dfFlu_actualTreatment <- dfFlu_treatment[dfFlu_treatment$Treatment == 1,]
perc_comply_flu <- nrow(dfFlu_actualTreatment)/nrow(dfFlu_treatment)


new_size_flu = (1/(perc_comply_flu)^2)*size_flu


type_group <- ifelse(dfFlu$TreatGroup ==1 & dfFlu$Treatment == 0, "Untreated treated",ifelse(dfFlu$TreatGroup ==1 & dfFlu$Treatment == 1, "Treated treated","control"))
dfFlu$type_group <- type_group

dfSummary <- dfFlu %>%
  group_by(type_group) %>% 
  summarise(
    
  )

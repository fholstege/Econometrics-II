# Packes required for subsequent analysis. P_load ensures these will be installed and loaded. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plm, 
               Formula,
               car,
               clubSandwich,
               lmtest)


# create df with treatment effects
dfTreatment <- data.frame(N_treated = c(100, 75, 25), N_control = c(100, 25, 75), Avg_Outcome_treated = c(9, 13,10), Avg_Outcome_control = c(7,8,9))
row.names(dfTreatment) <- c("Purple", "Blue", "Green")

# average treatment effect per group
ATE_group <- dfTreatment$Avg_Outcome_treated - dfTreatment$Avg_Outcome_control

